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

    let age_mli_file = open_out ("props/uProp" ^ (String.capitalize P.name) ^ ".mli") in

    let () = output_string age_mli_file ("type t = " ^ P.type_name ^ "\n") in
    let () = output_string age_mli_file ("val get : UCoreLib.UChar.uchar -> " ^ P.type_name  ^ "\n") in

    let () = flush_all () in
    let () = close_out age_mli_file in

    let age_ml_file = open_out "props/uPropAge.ml" in

    let () = output_string age_ml_file ("type t = " ^ P.type_name ^ "\n") in
    let () = output_string age_ml_file ("let tbl = Marshal.from_string \"" ^ (String.escaped tbl_mar) ^ "\" 0 \n") in
    let () = output_string age_ml_file "let get u = UCoreLib.UCharTbl.get tbl u\n" in

    let () = flush_all () in
    let () = close_out age_ml_file in
    let () = Printf.printf " done.\n"; flush_all () in
    ()
end

(* age *)

module AgeSig = struct 
  type t = [ `Unassigned | `Version of int * int ]

  let property = Uucd.age
  let default = `Unassigned
  let name = "age"
  let type_name = "[ `Unassigned | `Version of int * int ]"
end

module Age = ProcProp(AgeSig)

let () = Age.run ()
