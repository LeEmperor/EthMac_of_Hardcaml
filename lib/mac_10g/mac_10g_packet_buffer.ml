(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "mac_10g_packet_buffer.ml" *)
(* Transactional packet byte ring with speculative writes, commit/rollback, a descriptor
   FIFO, and a stall-stable 64-bit read interface.

   The byte store is split into eight banks so one AXI beat can be written and read per
   cycle without a multi-write-port RAM.

   For example purposes, a 8192 depth bytes Config.value will be used.
*)

open! Core
open! Hardcaml
open! Signal

module type Config = sig
  val depth_bytes : int (* *)
  val descriptor_capacity : int (* how many entries the descriptor FIFO holds *)
  val error_width : int
end

module Make (Config : Config) = struct
  let () =
    if Config.depth_bytes < 16 || not (Int.is_pow2 Config.depth_bytes)
    then
      invalid_arg
        "Mac_10g_packet_buffer: depth_bytes must be a power of two and at least 16";
    if Config.descriptor_capacity < 2 || not (Int.is_pow2 Config.descriptor_capacity)
    then
      invalid_arg
        "Mac_10g_packet_buffer: descriptor_capacity must be a power of two and at least 2";
    if Config.error_width < 1
    then invalid_arg "Mac_10g_packet_buffer: error_width must be positive"
  ;;

  (* address based on depth -> standard parameterization *)
  (* byte location in the buffer *)
  (* $clog2(8192) = 13 *)
  let address_width = Int.ceil_log2 Config.depth_bytes

  (* byte-width memory bank addressing *)
  (* assume 1kB banks for the 8192B system; 8 banks of 1kB *)
  let bank_address_width = address_width - 3

  (* where was num_bits_to_represent all my life? *)
  let length_width = num_bits_to_represent Config.depth_bytes
  (* quantity of bytes -> 14 *)
  (* this is 14 because fullness needs the extra bit *)

  let descriptor_address_width = Int.ceil_log2 Config.descriptor_capacity
  let descriptor_count_width = num_bits_to_represent Config.descriptor_capacity

  module I = struct
    type 'a t =
      { clock_i : 'a
      ; reset_i : 'a
      ; write_data_i : 'a [@bits 64]
      ; write_keep_i : 'a [@bits 8]
      ; write_valid_i : 'a
      ; commit_i : 'a
      ; rollback_i : 'a
      ; commit_error_i : 'a [@bits Config.error_width]
      ; read_ready_i : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { write_ready_o : 'a
      ; commit_ready_o : 'a
      ; current_frame_length_o : 'a [@bits length_width]
      ; read_data_o : 'a [@bits 64]
      ; read_keep_o : 'a [@bits 8]
      ; read_valid_o : 'a
      ; read_last_o : 'a
      ; read_error_o : 'a [@bits Config.error_width]
      ; bytes_used_o : 'a [@bits length_width]
      ; descriptors_used_o : 'a [@bits descriptor_count_width]
      }
    [@@deriving hardcaml]
  end

  [@@@ocamlformat "disable"]

  let create (scope : Scope.t) (i : _ I.t) : _ O.t =

    (* spec *)
    let spec = Reg_spec.create ~clock:i.clock_i ~clear:i.reset_i () in

    (* helpers *)
    let ( -- ) = Scope.naming scope in
    let reg_var width       = Always.Variable.reg ~enable:vdd ~width spec in

    (* if i want internal tagging on these then the I_Wires and I_Regs idiom might be better *)
    let speculative_pointer       = reg_var address_width in
    let committed_write_pointer   = reg_var address_width in
    let read_pointer              = reg_var address_width in

    (* bytes in the in-flight frame *)
    let speculative_length        = reg_var length_width in

    (* total occupancy counter *)
    let bytes_used                = reg_var length_width in

    (* how far into the current descriptor we've gotten *)
    let descriptor_write_pointer  = reg_var descriptor_address_width in
    let descriptor_read_pointer   = reg_var descriptor_address_width in
    let descriptors_used          = reg_var descriptor_count_width in

    (* how far into the current frame are we? kinda like a cursor *)
    (* when we start reading a frame, this is 0; and each beat that we pass
      it increments by 8; *)
    let read_offset               = reg_var length_width in

    (* of the write thats going into the buffer, how many bytes are being kept?
       turn the keep mask into an int -> popcount(write_keep_i)
      4b value accumulator
    *)
    let write_count = Mac_10g_axis.keep_byte_count i.write_keep_i in

    (* extend into the length param's width ; 4 > 14 *)
    let write_count_ext = uresize write_count ~width:length_width in

    (* consider the example 8k depth, with 64b per entry *)
    (* for example with 20 bytes stored, a cpacity of 100, and a pending commit of 45, this should be 65 *)
    let free_bytes =
      of_int_trunc ~width:length_width Config.depth_bytes -:
      bytes_used.value
    in

    (* as long as the write count amount is less than the free storage,
      we are "ready" to accept *)
    let write_ready = (write_count_ext <=: free_bytes) -- "write_ready" in

    (* helper for the transaction interface *)
    let write_accepted =
      (i.write_valid_i (* valid *)
      &: write_ready (* ready *)
      &: ~:(i.rollback_i) (* not rolling back on this beat *)
      &: (write_count <>:. 0)) -- "write_accepted" (*bruh*)
    in

    (* self explanatory-ish *)
    let (accepted_count : t) =
      mux2
        (* is the write accepted? *)
        write_accepted

        (* yes - the count is the write_count *)
        write_count

        (* else zero *)
        (zero 4)
    in

    (* zero-pad *)
    let accepted_count_ext = uresize accepted_count ~width:length_width in

    (* what the end will be upon a commit? *)
    let speculative_end =
      speculative_pointer.value +: (uresize accepted_count ~width:address_width)
    in

    (* lenght of what? *)
    (* because we compute this here - which takes into account the beat happening right now
       commit can be on the final beat of the frame
    *)
    let length_after_write =
      speculative_length.value +:
      accepted_count_ext
    in (* extra speculative items *)

    (* chiller *)
    let descriptor_write_enable = Signal.wire 1 in

    (* The descriptor FIFO is one logical record { length; error } per committed frame.
       Hardcaml memories store a flat bit vector, so it is two RAMs that share their
       addressing and enable, stepping in lockstep. [descriptor_ram] is that shared
       shape: sync write on [descriptor_write_enable], async read at the FIFO head, and
       one read port whose output is the record field for the frame being read out.
       Data width is inferred from [write_data]. *)
    let descriptor_ram ~name ~write_data =
      (* heres the actual memory instantiation *)
      let read_ports =
        multiport_memory
          ~name
          Config.descriptor_capacity
          ~write_ports:
            [| { Write_port.write_clock = i.clock_i
               ; write_address = descriptor_write_pointer.value
               ; write_enable = descriptor_write_enable
               ; write_data
               }
            |]
          ~read_addresses:[| descriptor_read_pointer.value |]
      in

      (* one read address in, so one read output back out *)
      (* this is doing mem[head] basically *)
      read_ports.(0)
    in

    (* head-of-FIFO fields: the frame the read side is currently draining *)
    (* fifo entries are descriptors, which have lengths; therefore we want
      the length of the head for the next read op*)
    let descriptor_length =
      descriptor_ram ~name:"descriptor_length" ~write_data:length_after_write
    in
    let descriptor_error =
      descriptor_ram ~name:"descriptor_error" ~write_data:i.commit_error_i
    in

    (* if it is zero, then we're not reading anything valid *)
    (* can be thought of read_valid = ~empty in normal FIFO designs *)
    let read_valid = descriptors_used.value <>:. 0 in

    (* descriptor_length is the head of the descriptor FIFO, and
        the difference between that and the read_offset is important for how much more
      of the frame that we need to eat;
    *)
    let remaining = descriptor_length -: read_offset.value in

    (* is this the last read? *)
    (* remaining <=. 8 means that this last beat will have all of the remaining payload
        may need to zero pad and calculate a partial tkeep on the AXI-Stream transaction
      *)
    let read_last =
      read_valid &:
      (remaining <=:. 8)
    in

    let read_byte_count =
      mux2
        (* are we in the last read beat of a frame? *)
        (remaining <=:. 8)

        (* yes - slice from the remaining count raw -> we might have 3B left,
          therefore need to know that were reading 3B in this beat *)
        (select remaining ~high:3 ~low:0)

        (* reading a full beat -> rawly 8 *)
        (of_int_trunc ~width:4 8)
    in

    (* generate keep mask of the number of bytes that we're reading in the 64b beat *)
    let read_keep = Mac_10g_axis.keep_of_byte_count read_byte_count in

    (* handshake *)
    let read_accepted = read_valid &: i.read_ready_i in

    (**)
    let accepted_read_count =
      mux2
        (* is the read accepted? *)
        read_accepted

        (* yes -> passthrough the read byte count (lir'e) *)
        read_byte_count

        (* or zero out *)
        (zero 4)
    in

    (* the descriptor has finally been used; does this need any special treatment related
      to commit and pullback? *)
    let pop_descriptor = read_accepted &: read_last in

    (* is there space? covers the was-just-full a cycle ago case *)
    let descriptor_space =
      descriptors_used.value <>:. Config.descriptor_capacity |: pop_descriptor
    in

    (* is there space && length of the pending write isnt 0 -> might be a level too many *)
    let (commit_ready : t) = descriptor_space &: (length_after_write <>:. 0) in

    (* handshake + ~rollback *)
    let commit_accepted = i.commit_i &: commit_ready &: ~:(i.rollback_i) in
    Signal.(descriptor_write_enable <-- commit_accepted); (* assign me! *)

    (* pointer_for_lane.(L) =
      the byte address in the ring where lane L's byte of this beat will land.

      for a given beat, we might not be writing all 64b of it;
      with this in mind, do we keep the gap in mind for the individual memory banks?

      no. each bank needs its own addr pointer then. this seems interesting;
      downthe line can we make tradeoffs on speed vs dead capacity?
    *)

    (* example depth_bytes = 8192
         -> address_width = 13
        say speculative_pointer = 254,
          write_keep_i = 0b0011_1111 (6 valid bytes, lanes 0–5):

    | lane | keep | prefix | pointer | bank | row |
    |---   |---   |---     |---      |---   |--- |
    | 0    | 1    | 0      | 254     | 6    | 31 |
    | 1    | 1    | 1      | 255     | 7    | 31 |
    | 2    | 1    | 2      | 256     | 0    | 32 |
    | 3    | 1    | 3      | 257     | 1    | 32 |
    | 4    | 1    | 4      | 258     | 2    | 32 |
    | 5    | 1    | 5      | 259     | 3    | 32 |
    | 6    | 0    | 6      | 260     | 4    | 32 | ← masked off
    | 7    | 0    | 6      | 260     | 4    | 32 | ← masked off

      with a 48b for example, on speculative = 254, we'd start with lanes 6 7 0 1 2 3
      lane candidates 4 5 would be calculated, but not actually written

      a following beat would actually start writing into those

      each candidate is a pointer into one of our individual memory banks that we have

        key detail is that a lane doesn't own a given bank, that can change
    *)
    (* think of this as 8 candidate addresses into the ring *)
    let pointer_for_lane =
      Array.init 8 ~f:(fun lane -> (* 8 lanes, indexable *)
        let prefix =
          List.range 0 lane (* from 0 to lane dis-inclusive; consider 64 -> [0:7] *)
          |> List.fold ~init:(zero 4) ~f:(fun count previous_lane ->
              count +: uresize (bit i.write_keep_i ~pos:previous_lane) ~width:4
            ) (* accumulate the number of lanes that have a keep asserted *)
            (* for example if i have 10B in the mem first, and then am going to rwite 20
            in the next write beat, add this to the speculative_pointer's value *)
        in (* the new pointer value is added with the number of items written into the mem? *)
        speculative_pointer.value +: (uresize prefix ~width:address_width)
        )
    in

    (* mem bank select *)
    let read_bank = select read_pointer.value ~high:2 ~low:0 in

    (* the other bits in the read pointer *)
    let read_row = select read_pointer.value ~high:(address_width - 1) ~low:3 in

    (*  *)
    let bank_outputs =
      List.init 8 ~f:(fun bank ->
        let write_matches =
          Array.init 8 ~f:(fun lane ->
            let destination_bank = select pointer_for_lane.(lane) ~high:2 ~low:0 in
            write_accepted &: bit i.write_keep_i ~pos:lane &: (destination_bank ==:. bank))
        in
        let write_enable = Array.reduce_exn write_matches ~f:( |: ) in
        let write_data =
          Array.foldi write_matches ~init:(zero 8) ~f:(fun lane data selected ->
            mux2
              selected
              (select i.write_data_i ~high:((8 * lane) + 7) ~low:(8 * lane))
              data)
        in
        let write_address =
          Array.foldi
            write_matches
            ~init:(zero bank_address_width)
            ~f:(fun lane address selected ->
              mux2
                selected
                (select pointer_for_lane.(lane) ~high:(address_width - 1) ~low:3)
                address)
        in
        let read_address =
          read_row +: uresize (read_bank >:. bank) ~width:bank_address_width
        in
        (multiport_memory
           ~name:(sprintf "byte_bank_%d" bank)
           (Config.depth_bytes / 8)
           ~write_ports:
             [| { Write_port.write_clock = i.clock_i
                ; write_address
                ; write_enable
                ; write_data
                }
             |]
           ~read_addresses:[| read_address |]).(0))
    in

    let read_data =
      concat_lsb (* scalar concate *)
        (List.init 8 ~f:(fun lane ->
           let bank = select (read_bank +:. lane) ~high:2 ~low:0 in
           mux bank bank_outputs))
    in

    let read_count_ext = uresize accepted_read_count ~width:length_width in
    let rollback_count = mux2 i.rollback_i speculative_length.value (zero length_width) in
    let next_bytes_used =
      bytes_used.value +: accepted_count_ext -: read_count_ext -: rollback_count
    in

    Always.(
      compile
        [ speculative_pointer
          <-- mux2 i.rollback_i committed_write_pointer.value speculative_end
        ; speculative_length
          <-- mux2
                (i.rollback_i |: commit_accepted)
                (zero length_width)
                length_after_write
        ; bytes_used <-- next_bytes_used
        ; when_
            commit_accepted
            [ committed_write_pointer <-- speculative_end
            ; descriptor_write_pointer <-- descriptor_write_pointer.value +:. 1
            ]
        ; when_
            read_accepted
            [ read_pointer
              <-- read_pointer.value +: uresize read_byte_count ~width:address_width
            ; if_
                read_last
                [ read_offset <--. 0
                ; descriptor_read_pointer <-- descriptor_read_pointer.value +:. 1
                ]
                [ read_offset <-- read_offset.value +: read_count_ext ]
            ]
        ; descriptors_used
          <-- mux
                (commit_accepted @: pop_descriptor)
                [ descriptors_used.value
                ; descriptors_used.value -:. 1
                ; descriptors_used.value +:. 1
                ; descriptors_used.value
                ]
        ]);
    { O.write_ready_o = write_ready
    ; commit_ready_o = commit_ready
    ; current_frame_length_o = speculative_length.value
    ; read_data_o = read_data
    ; read_keep_o = mux2 read_valid read_keep (zero 8)
    ; read_valid_o = read_valid
    ; read_last_o = read_last
    ; read_error_o = mux2 read_valid descriptor_error (zero Config.error_width)
    ; bytes_used_o = bytes_used.value
    ; descriptors_used_o = descriptors_used.value
    }

  [@@@ocamlformat "enable"]

  let hierarchical ?instance scope i =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical
      ?instance
      ~scope
      ~name:
        (sprintf
           "mac_10g_packet_buffer_%d_%d"
           Config.depth_bytes
           Config.descriptor_capacity)
      create
      i
  ;;
end
