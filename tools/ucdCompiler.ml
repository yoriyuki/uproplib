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

(* age *)

let () = Printf.printf "Processing age....."; flush_all ()

type age = [ `Unassigned | `Version of int * int ]

let map = ref (UMap.empty ((=) : age -> age -> bool))

let () = for i = 0 to 0x10ffff do
  if not (Uucd.is_scalar_value i) then () else begin
    match Uucd.cp_prop ucd i Uucd.age with
      Some p -> 
	map := UMap.add (UChar.of_int_exn i) p !map
    | None -> ()
  end
done

module PHash = struct
  type t = age
  let equal : age -> age -> bool = (=)
  let hash = Hashtbl.hash
end

module PTbl = UCharTbl.Make(PHash)

let tbl = PTbl.of_map `Unassigned !map

let tbl_mar = Marshal.to_string (tbl : PTbl.t) []
(*let map_mar = Marshal.to_string !map []*)

let age_mli_file = open_out "props/uPropAge.mli"

let () = output_string age_mli_file "val age : UCoreLib.UChar.uchar -> [ `Unassigned | `Version of int * int ]\n"

let () = flush_all ()
let () = close_out age_mli_file

let age_ml_file = open_out "props/uPropAge.ml"

let () = output_string age_ml_file ("let tbl = Marshal.from_string \"" ^ (String.escaped tbl_mar) ^ "\" 0 \n")
let () = output_string age_ml_file "let age u = UCoreLib.UCharTbl.get tbl u\n"

let () = flush_all ()
let () = close_out age_ml_file
let () = Printf.printf " done.\n"; flush_all ()
