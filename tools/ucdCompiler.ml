open UCoreLib

let get_ucd inf = try 
  let ic = if inf = "-" then stdin else open_in inf in
  let d = Uucd.decoder (`Channel ic) in 
  match Uucd.decode d with 
  | `Ok db -> db 
  | `Error e -> 
    let (l0, c0), (l1, c1) = Uucd.decoded_range d in
    Printf.eprintf "%s:%d.%d-%d.%d: %s\n%!" inf l0 c0 l1 c1 e; 
    exit 1
with Sys_error e -> Printf.eprintf "%s\n%!" e; exit 1

let () = Printf.printf "Reading....."; flush_all ()

let ucd = get_ucd "ucd/ucd.all.grouped.xml"

let () = Printf.printf " done.\n"; flush_all ()

module type PropSig = sig
  type t

  val property : t Uucd.prop
  val default : t

  val name : string
  val type_name : string
end

module ProcProp (P : PropSig) = struct
  module PHash = struct
    type t = P.t
    let equal :  P.t -> P.t -> bool = (=)
    let hash = Hashtbl.hash
  end

  module PTbl = UCharTbl.Make(PHash)

  let run () =
    let () = (Printf.printf "Processing %s....." P.name); flush_all () in

    let map = ref (UMap.empty ((=) : P.t -> P.t -> bool)) in
	
    let () = for i = 0 to 0x10ffff do
      if not (Uucd.is_scalar_value i) then () else begin
	match Uucd.cp_prop ucd i P.property with
	  Some p -> 
	    map := UMap.add (UChar.of_int_exn i) p !map
	| None -> ()
      end
    done in

    let tbl = PTbl.of_map P.default !map in

    let tbl_mar = Marshal.to_string (tbl : PTbl.t) [] in
(*let map_mar = Marshal.to_string !map []*)

    let prop_mli_file = open_out ("props/uProp" ^ P.name ^ ".mli") in

    let () = output_string prop_mli_file ("type t = " ^ P.type_name ^ "\n") in
    let () = output_string prop_mli_file ("val get : UCoreLib.UChar.uchar -> " ^ P.type_name  ^ "\n") in

    let () = flush_all () in
    let () = close_out prop_mli_file in

    let prop_ml_file = open_out ("props/uProp" ^ P.name ^ ".ml") in

    let () = output_string prop_ml_file ("type t = " ^ P.type_name ^ "\n") in
    let () = output_string prop_ml_file ("let tbl = Marshal.from_string \"" ^ (String.escaped tbl_mar) ^ "\" 0 \n") in
    let () = output_string prop_ml_file "let get u = UCoreLib.UCharTbl.get tbl u\n" in

    let () = flush_all () in
    let () = close_out prop_ml_file in
    let () = Printf.printf " done.\n"; flush_all () in
    ()
end

module type BoolPropSig = sig

  val property : bool Uucd.prop

  val name : string
end

module ProcBoolProp (P : BoolPropSig) = struct

  let run () =
    let () = (Printf.printf "Processing %s....." P.name); flush_all () in

    let set = ref USet.empty in
	
    let () = for i = 0 to 0x10ffff do
      if not (Uucd.is_scalar_value i) then () else begin
	match Uucd.cp_prop ucd i P.property with
	  Some true -> set  := USet.add (UChar.of_int_exn i) !set
	| _ -> ()
      end
    done in

    let tbl = UCharTbl.Bool.of_set !set in

    let tbl_mar = Marshal.to_string (tbl : UCharTbl.Bool.t) [] in
(*let map_mar = Marshal.to_string !map []*)

    let prop_mli_file = open_out ("props/uProp" ^ P.name ^ ".mli") in

    let () = output_string prop_mli_file ("val get : UCoreLib.UChar.uchar -> bool \n") in

    let () = flush_all () in
    let () = close_out prop_mli_file in

    let prop_ml_file = open_out ("props/uProp" ^ P.name ^ ".ml") in

    let () = output_string prop_ml_file ("let tbl = Marshal.from_string \"" ^ (String.escaped tbl_mar) ^ "\" 0 \n") in
    let () = output_string prop_ml_file "let get u = UCoreLib.UCharTbl.get tbl u\n" in

    let () = flush_all () in
    let () = close_out prop_ml_file in
    let () = Printf.printf " done.\n"; flush_all () in
    ()
end

(* age *)

module AgeStr = struct 
  type t = [ `Unassigned | `Version of int * int ]

  let property = Uucd.age
  let default = `Unassigned
  let name = "Age"
  let type_name = "[ `Unassigned | `Version of int * int ]"
end

module Age = ProcProp(AgeStr)

let () = Age.run ()

(* alphabetic *)
module AlphabeticStr = struct
  let property = Uucd.alphabetic
  let name = "Alphabetic"
end

module Alphabetic = ProcBoolProp(AlphabeticStr)
let () = Alphabetic.run ()

(* ascii_hex_digit *)
module AsciiHexDigitStr = struct
  let property = Uucd.ascii_hex_digit
  let name = "AsciiHexDigit"
end

module AsciiHexDigit = ProcBoolProp(AsciiHexDigitStr)
let () = AsciiHexDigit.run ()

(* bidi_class *)
module BidiClassStr = struct 
  type t = [ `AL
       | `AN
       | `B
       | `BN
       | `CS
       | `EN
       | `ES
       | `ET
       | `FSI
       | `L
       | `LRE
       | `LRI
       | `LRO
       | `NSM
       | `ON
       | `PDF
       | `PDI
       | `R
       | `RLE
       | `RLI
       | `RLO
       | `S
       | `WS ]

  let property = Uucd.bidi_class
  let default = `BN
  let name = "BidiClass"
  let type_name = "[ `AL
       | `AN
       | `B
       | `BN
       | `CS
       | `EN
       | `ES
       | `ET
       | `FSI
       | `L
       | `LRE
       | `LRI
       | `LRO
       | `NSM
       | `ON
       | `PDF
       | `PDI
       | `R
       | `RLE
       | `RLI
       | `RLO
       | `S
       | `WS ]"
end

module BidiClass = ProcProp(BidiClassStr)

let () = BidiClass.run ()
