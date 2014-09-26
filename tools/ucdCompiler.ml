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

module type PropComplexSig = sig
  type t

  val property : t Uucd.prop
  val default_value : Uucd.cp -> t

  val name : string
  val type_name : string
end

module ProcComplexProp (P : PropComplexSig) = struct
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
	| None -> 
	    if P.default_value i == P.default_value 0 then () else
	    map := UMap.add (UChar.of_int_exn i) (P.default_value i) !map
      end
    done in

    let tbl = PTbl.of_map (P.default_value 0) !map in

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

module type PropSig = sig
  type t

  val property : t Uucd.prop
  val default : t

  val name : string
  val type_name : string
end

module ProcProp (P : PropSig) = struct
  module PComplex = struct 
    include P
    let default_value _ = default
  end

  include ProcComplexProp(PComplex)
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
    let () = output_string prop_ml_file "let get u = UCoreLib.UCharTbl.Bool.get tbl u\n" in

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

  let default_value cp =
    if cp >= 0x0600 && cp <= 0x07bf
  || cp >= 0x08A0 && cp <= 0x08FF 
  || cp >= 0xFB50 && cp <= 0xFDCF
  || cp >= 0xFDF0 && cp <= 0xFDFF
  || cp >= 0xFE70 && cp <= 0xFEFF
  || cp >= 0x0001EE00 && cp <= 0x0001EEFF then `AL else
      if cp >= 0x0590 && cp <= 0x05FF
  || cp >= 0x07C0 && cp <= 0x089F
  || cp >= 0xFB1D && cp <= 0xFB4F
  || cp >= 0x00010800 && cp <= 0x00010FFF
  || cp >= 0x0001E800 && cp <= 0x0001EDFF
  || cp >= 0x0001EF00 && cp <= 0x0001EFFF then `R else
	if cp >= 0x20A0 && cp <= 0x20CF then `ET else
	match Uucd.cp_prop ucd cp Uucd.default_ignorable_code_point,
	  Uucd.cp_prop ucd cp Uucd.noncharacter_code_point with
	  Some true, _ | _,  Some true -> `BN
	  | _ -> `L
	
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

module BidiClass = ProcComplexProp(BidiClassStr)

let () = BidiClass.run ()

(* Bidi Control *)

module BidiControlStr = struct
  let property = Uucd.bidi_control
  let name = "BidiControl"
end

module BidiControl = ProcBoolProp(BidiControlStr)
let () = BidiControl.run ()

(* Bidi Mirrored *)

module BidiMirroredStr = struct
  let property = Uucd.bidi_mirrored
  let name = "BidiMirrored"
end

module BidiMirrored = ProcBoolProp(BidiMirroredStr)
let () = BidiMirrored.run ()

(* Bidi Mirroring Glyph *)

module BidiMirroringGlyphStr = struct 
  type t = int option

  let property = Uucd.bidi_mirroring_glyph
  let default = None
  let name = "BidiMirroringGlyph"
  let type_name = "UCoreLib.UChar.t option"
end

module BidiMirroringGlyph = ProcProp(BidiMirroringGlyphStr)

let () = BidiMirroringGlyph.run ()

(* bidi_paired_bracket *) 

module BidiPairedBracketStr = struct 
  type t = [ `Cp of int | `Self ]

  let property = Uucd.bidi_paired_bracket
  let default = `Self
  let name = "BidiPairedBracket"
  let type_name = "[ `Cp of int | `Self ]"
end

module BidiPairedBracket = ProcProp(BidiPairedBracketStr)

let () = BidiPairedBracket.run ()

(* bidi_paired_bracket_type *)

module BidiPairedBracketTypeStr = struct 
  type t = [ `C | `N | `O ]

  let property = Uucd.bidi_paired_bracket_type
  let default = `N
  let name = "BidiPairedBracketType"
  let type_name = "[ `C | `N | `O ]"
end

module BidiPairedBracketType = ProcProp(BidiPairedBracketTypeStr)

let () = BidiPairedBracketType.run ()

(* block *)

module BlockStr = struct 
  type t =  [ `ASCII
       | `Aegean_Numbers
       | `Alchemical
       | `Alphabetic_PF
       | `Ancient_Greek_Music
       | `Ancient_Greek_Numbers
       | `Ancient_Symbols
       | `Arabic
       | `Arabic_Ext_A
       | `Arabic_Math
       | `Arabic_PF_A
       | `Arabic_PF_B
       | `Arabic_Sup
       | `Armenian
       | `Arrows
       | `Avestan
       | `Balinese
       | `Bamum
       | `Bamum_Sup
       | `Bassa_Vah
       | `Batak
       | `Bengali
       | `Block_Elements
       | `Bopomofo
       | `Bopomofo_Ext
       | `Box_Drawing
       | `Brahmi
       | `Braille
       | `Buginese
       | `Buhid
       | `Byzantine_Music
       | `CJK
       | `CJK_Compat
       | `CJK_Compat_Forms
       | `CJK_Compat_Ideographs
       | `CJK_Compat_Ideographs_Sup
       | `CJK_Ext_A
       | `CJK_Ext_B
       | `CJK_Ext_C
       | `CJK_Ext_D
       | `CJK_Radicals_Sup
       | `CJK_Strokes
       | `CJK_Symbols
       | `Carian
       | `Caucasian_Albanian
       | `Chakma
       | `Cham
       | `Cherokee
       | `Compat_Jamo
       | `Control_Pictures
       | `Coptic
       | `Coptic_Epact_Numbers
       | `Counting_Rod
       | `Cuneiform
       | `Cuneiform_Numbers
       | `Currency_Symbols
       | `Cypriot_Syllabary
       | `Cyrillic
       | `Cyrillic_Ext_A
       | `Cyrillic_Ext_B
       | `Cyrillic_Sup
       | `Deseret
       | `Devanagari
       | `Devanagari_Ext
       | `Diacriticals
       | `Diacriticals_Ext
       | `Diacriticals_For_Symbols
       | `Diacriticals_Sup
       | `Dingbats
       | `Domino
       | `Duployan
       | `Egyptian_Hieroglyphs
       | `Elbasan
       | `Emoticons
       | `Enclosed_Alphanum
       | `Enclosed_Alphanum_Sup
       | `Enclosed_CJK
       | `Enclosed_Ideographic_Sup
       | `Ethiopic
       | `Ethiopic_Ext
       | `Ethiopic_Ext_A
       | `Ethiopic_Sup
       | `Geometric_Shapes
       | `Geometric_Shapes_Ext
       | `Georgian
       | `Georgian_Sup
       | `Glagolitic
       | `Gothic
       | `Grantha
       | `Greek
       | `Greek_Ext
       | `Gujarati
       | `Gurmukhi
       | `Half_And_Full_Forms
       | `Half_Marks
       | `Hangul
       | `Hanunoo
       | `Hebrew
       | `High_PU_Surrogates
       | `High_Surrogates
       | `Hiragana
       | `IDC
       | `IPA_Ext
       | `Imperial_Aramaic
       | `Indic_Number_Forms
       | `Inscriptional_Pahlavi
       | `Inscriptional_Parthian
       | `Jamo
       | `Jamo_Ext_A
       | `Jamo_Ext_B
       | `Javanese
       | `Kaithi
       | `Kana_Sup
       | `Kanbun
       | `Kangxi
       | `Kannada
       | `Katakana
       | `Katakana_Ext
       | `Kayah_Li
       | `Kharoshthi
       | `Khmer
       | `Khmer_Symbols
       | `Khojki
       | `Khudawadi
       | `Lao
       | `Latin_1_Sup
       | `Latin_Ext_A
       | `Latin_Ext_Additional
       | `Latin_Ext_B
       | `Latin_Ext_C
       | `Latin_Ext_D
       | `Latin_Ext_E
       | `Lepcha
       | `Letterlike_Symbols
       | `Limbu
       | `Linear_A
       | `Linear_B_Ideograms
       | `Linear_B_Syllabary
       | `Lisu
       | `Low_Surrogates
       | `Lycian
       | `Lydian
       | `Mahajani
       | `Mahjong
       | `Malayalam
       | `Mandaic
       | `Manichaean
       | `Math_Alphanum
       | `Math_Operators
       | `Meetei_Mayek
       | `Meetei_Mayek_Ext
       | `Mende_Kikakui
       | `Meroitic_Cursive
       | `Meroitic_Hieroglyphs
       | `Miao
       | `Misc_Arrows
       | `Misc_Math_Symbols_A
       | `Misc_Math_Symbols_B
       | `Misc_Pictographs
       | `Misc_Symbols
       | `Misc_Technical
       | `Modi
       | `Modifier_Letters
       | `Modifier_Tone_Letters
       | `Mongolian
       | `Mro
       | `Music
       | `Myanmar
       | `Myanmar_Ext_A
       | `Myanmar_Ext_B
       | `NB
       | `NKo
       | `Nabataean
       | `New_Tai_Lue
       | `Number_Forms
       | `OCR
       | `Ogham
       | `Ol_Chiki
       | `Old_Italic
       | `Old_North_Arabian
       | `Old_Permic
       | `Old_Persian
       | `Old_South_Arabian
       | `Old_Turkic
       | `Oriya
       | `Ornamental_Dingbats
       | `Osmanya
       | `PUA
       | `Pahawh_Hmong
       | `Palmyrene
       | `Pau_Cin_Hau
       | `Phags_Pa
       | `Phaistos
       | `Phoenician
       | `Phonetic_Ext
       | `Phonetic_Ext_Sup
       | `Playing_Cards
       | `Psalter_Pahlavi
       | `Punctuation
       | `Rejang
       | `Rumi
       | `Runic
       | `Samaritan
       | `Saurashtra
       | `Sharada
       | `Shavian
       | `Shorthand_Format_Controls
       | `Siddham
       | `Sinhala
       | `Sinhala_Archaic_Numbers
       | `Small_Forms
       | `Sora_Sompeng
       | `Specials
       | `Sundanese
       | `Sundanese_Sup
       | `Sup_Arrows_A
       | `Sup_Arrows_B
       | `Sup_Arrows_C
       | `Sup_Math_Operators
       | `Sup_PUA_A
       | `Sup_PUA_B
       | `Sup_Punctuation
       | `Super_And_Sub
       | `Syloti_Nagri
       | `Syriac
       | `Tagalog
       | `Tagbanwa
       | `Tags
       | `Tai_Le
       | `Tai_Tham
       | `Tai_Viet
       | `Tai_Xuan_Jing
       | `Takri
       | `Tamil
       | `Telugu
       | `Thaana
       | `Thai
       | `Tibetan
       | `Tifinagh
       | `Tirhuta
       | `Transport_And_Map
       | `UCAS
       | `UCAS_Ext
       | `Ugaritic
       | `VS
       | `VS_Sup
       | `Vai
       | `Vedic_Ext
       | `Vertical_Forms
       | `Warang_Citi
       | `Yi_Radicals
       | `Yi_Syllables
       | `Yijing ] 

  let property = Uucd.block
  let default = `NB
  let name = "Block"
  let type_name = " [ `ASCII
       | `Aegean_Numbers
       | `Alchemical
       | `Alphabetic_PF
       | `Ancient_Greek_Music
       | `Ancient_Greek_Numbers
       | `Ancient_Symbols
       | `Arabic
       | `Arabic_Ext_A
       | `Arabic_Math
       | `Arabic_PF_A
       | `Arabic_PF_B
       | `Arabic_Sup
       | `Armenian
       | `Arrows
       | `Avestan
       | `Balinese
       | `Bamum
       | `Bamum_Sup
       | `Bassa_Vah
       | `Batak
       | `Bengali
       | `Block_Elements
       | `Bopomofo
       | `Bopomofo_Ext
       | `Box_Drawing
       | `Brahmi
       | `Braille
       | `Buginese
       | `Buhid
       | `Byzantine_Music
       | `CJK
       | `CJK_Compat
       | `CJK_Compat_Forms
       | `CJK_Compat_Ideographs
       | `CJK_Compat_Ideographs_Sup
       | `CJK_Ext_A
       | `CJK_Ext_B
       | `CJK_Ext_C
       | `CJK_Ext_D
       | `CJK_Radicals_Sup
       | `CJK_Strokes
       | `CJK_Symbols
       | `Carian
       | `Caucasian_Albanian
       | `Chakma
       | `Cham
       | `Cherokee
       | `Compat_Jamo
       | `Control_Pictures
       | `Coptic
       | `Coptic_Epact_Numbers
       | `Counting_Rod
       | `Cuneiform
       | `Cuneiform_Numbers
       | `Currency_Symbols
       | `Cypriot_Syllabary
       | `Cyrillic
       | `Cyrillic_Ext_A
       | `Cyrillic_Ext_B
       | `Cyrillic_Sup
       | `Deseret
       | `Devanagari
       | `Devanagari_Ext
       | `Diacriticals
       | `Diacriticals_Ext
       | `Diacriticals_For_Symbols
       | `Diacriticals_Sup
       | `Dingbats
       | `Domino
       | `Duployan
       | `Egyptian_Hieroglyphs
       | `Elbasan
       | `Emoticons
       | `Enclosed_Alphanum
       | `Enclosed_Alphanum_Sup
       | `Enclosed_CJK
       | `Enclosed_Ideographic_Sup
       | `Ethiopic
       | `Ethiopic_Ext
       | `Ethiopic_Ext_A
       | `Ethiopic_Sup
       | `Geometric_Shapes
       | `Geometric_Shapes_Ext
       | `Georgian
       | `Georgian_Sup
       | `Glagolitic
       | `Gothic
       | `Grantha
       | `Greek
       | `Greek_Ext
       | `Gujarati
       | `Gurmukhi
       | `Half_And_Full_Forms
       | `Half_Marks
       | `Hangul
       | `Hanunoo
       | `Hebrew
       | `High_PU_Surrogates
       | `High_Surrogates
       | `Hiragana
       | `IDC
       | `IPA_Ext
       | `Imperial_Aramaic
       | `Indic_Number_Forms
       | `Inscriptional_Pahlavi
       | `Inscriptional_Parthian
       | `Jamo
       | `Jamo_Ext_A
       | `Jamo_Ext_B
       | `Javanese
       | `Kaithi
       | `Kana_Sup
       | `Kanbun
       | `Kangxi
       | `Kannada
       | `Katakana
       | `Katakana_Ext
       | `Kayah_Li
       | `Kharoshthi
       | `Khmer
       | `Khmer_Symbols
       | `Khojki
       | `Khudawadi
       | `Lao
       | `Latin_1_Sup
       | `Latin_Ext_A
       | `Latin_Ext_Additional
       | `Latin_Ext_B
       | `Latin_Ext_C
       | `Latin_Ext_D
       | `Latin_Ext_E
       | `Lepcha
       | `Letterlike_Symbols
       | `Limbu
       | `Linear_A
       | `Linear_B_Ideograms
       | `Linear_B_Syllabary
       | `Lisu
       | `Low_Surrogates
       | `Lycian
       | `Lydian
       | `Mahajani
       | `Mahjong
       | `Malayalam
       | `Mandaic
       | `Manichaean
       | `Math_Alphanum
       | `Math_Operators
       | `Meetei_Mayek
       | `Meetei_Mayek_Ext
       | `Mende_Kikakui
       | `Meroitic_Cursive
       | `Meroitic_Hieroglyphs
       | `Miao
       | `Misc_Arrows
       | `Misc_Math_Symbols_A
       | `Misc_Math_Symbols_B
       | `Misc_Pictographs
       | `Misc_Symbols
       | `Misc_Technical
       | `Modi
       | `Modifier_Letters
       | `Modifier_Tone_Letters
       | `Mongolian
       | `Mro
       | `Music
       | `Myanmar
       | `Myanmar_Ext_A
       | `Myanmar_Ext_B
       | `NB
       | `NKo
       | `Nabataean
       | `New_Tai_Lue
       | `Number_Forms
       | `OCR
       | `Ogham
       | `Ol_Chiki
       | `Old_Italic
       | `Old_North_Arabian
       | `Old_Permic
       | `Old_Persian
       | `Old_South_Arabian
       | `Old_Turkic
       | `Oriya
       | `Ornamental_Dingbats
       | `Osmanya
       | `PUA
       | `Pahawh_Hmong
       | `Palmyrene
       | `Pau_Cin_Hau
       | `Phags_Pa
       | `Phaistos
       | `Phoenician
       | `Phonetic_Ext
       | `Phonetic_Ext_Sup
       | `Playing_Cards
       | `Psalter_Pahlavi
       | `Punctuation
       | `Rejang
       | `Rumi
       | `Runic
       | `Samaritan
       | `Saurashtra
       | `Sharada
       | `Shavian
       | `Shorthand_Format_Controls
       | `Siddham
       | `Sinhala
       | `Sinhala_Archaic_Numbers
       | `Small_Forms
       | `Sora_Sompeng
       | `Specials
       | `Sundanese
       | `Sundanese_Sup
       | `Sup_Arrows_A
       | `Sup_Arrows_B
       | `Sup_Arrows_C
       | `Sup_Math_Operators
       | `Sup_PUA_A
       | `Sup_PUA_B
       | `Sup_Punctuation
       | `Super_And_Sub
       | `Syloti_Nagri
       | `Syriac
       | `Tagalog
       | `Tagbanwa
       | `Tags
       | `Tai_Le
       | `Tai_Tham
       | `Tai_Viet
       | `Tai_Xuan_Jing
       | `Takri
       | `Tamil
       | `Telugu
       | `Thaana
       | `Thai
       | `Tibetan
       | `Tifinagh
       | `Tirhuta
       | `Transport_And_Map
       | `UCAS
       | `UCAS_Ext
       | `Ugaritic
       | `VS
       | `VS_Sup
       | `Vai
       | `Vedic_Ext
       | `Vertical_Forms
       | `Warang_Citi
       | `Yi_Radicals
       | `Yi_Syllables
       | `Yijing ]"
end

module Block = ProcProp(BlockStr)

let () = Block.run ()

(* canonical_combining_class  *)

module CanonicalCombiningClassStr = struct 
  type t = int

  let property = Uucd.canonical_combining_class 
  let default = 0
  let name = "CanonicalCombiningClass"
  let type_name = "int"
end

module CanonicalCombiningClass  = ProcProp(CanonicalCombiningClassStr)

let () = CanonicalCombiningClass.run ()

(* Bidi Mirrored *)

module CasedStr = struct
  let property = Uucd.cased
  let name = "Cased"
end

module Cased = ProcBoolProp(CasedStr)
let () = Cased.run ()

(* case_folding *)

module CaseFoldingStr = struct 
  type t = [ `Cps of int list | `Self ]

  let property = Uucd.case_folding 
  let default = `Self
  let name = "CaseFolding"
  let type_name = "[`Cps of int list | `Self]"
end

module CaseFolding = ProcProp(CaseFoldingStr)

let () = CaseFolding.run ()

(* case_ignorable *)

module CaseIgnorableStr = struct
  let property = Uucd.case_ignorable
  let name = "CaseIgnorable"
end

module CaseIgnorable = ProcBoolProp(CaseIgnorableStr)
let () = CaseIgnorable.run ()

(* changes_when_casefolded *)

module ChangesWhenCasefoldedStr = struct
  let property = Uucd.changes_when_casefolded
  let name = "ChangesWhenCasefolded"
end

module ChangesWhenCasefolded = ProcBoolProp(ChangesWhenCasefoldedStr)
let () = ChangesWhenCasefolded.run ()

(* changes_when_casemapped *)

module ChangesWhenCasemappedStr = struct
  let property = Uucd.changes_when_casemapped
  let name = "ChangesWhenCasemapped"
end

module ChangesWhenCasemapped = ProcBoolProp(ChangesWhenCasemappedStr)
let () = ChangesWhenCasemapped.run ()

(* changes_when_lowercased *)

module ChangesWhenLowercasedStr = struct
  let property = Uucd.changes_when_lowercased
  let name = "ChangesWhenLowercased"
end

module ChangesWhenLowercased = ProcBoolProp(ChangesWhenLowercasedStr)
let () = ChangesWhenLowercased.run ()

(* changes_when_nfkc_casefolded *)

module ChangesWhenNfkcCasefoldedStr = struct
  let property = Uucd.changes_when_nfkc_casefolded
  let name = "ChangesWhenNfkcCasefolded"
end

module ChangesWhenNfkcCasefolded = ProcBoolProp(ChangesWhenNfkcCasefoldedStr)
let () = ChangesWhenNfkcCasefolded.run ()

(* changes_when_titlecased *)

module ChangesWhenTitlecasedStr = struct
  let property = Uucd.changes_when_titlecased
  let name = "ChangesWhenTitlecased"
end

module ChangesWhenTitlecased = ProcBoolProp(ChangesWhenTitlecasedStr)
let () = ChangesWhenTitlecased.run ()

(* changes_when_uppercased *)

module ChangesWhenUppercasedStr = struct
  let property = Uucd.changes_when_uppercased
  let name = "ChangesWhenUppercased"
end

module ChangesWhenUppercased = ProcBoolProp(ChangesWhenUppercasedStr)
let () = ChangesWhenUppercased.run ()

(* composition_exclusion *)

module CompositionExclusionStr = struct
  let property = Uucd.composition_exclusion
  let name = "CompositionExclusion"
end

module CompositionExclusion = ProcBoolProp(CompositionExclusionStr)
let () = CompositionExclusion.run ()

(* dash *)

module DashStr = struct
  let property = Uucd.dash
  let name = "Dash"
end

module Dash = ProcBoolProp(DashStr)
let () = Dash.run ()

(* decomposition_mapping *)

module DecompositionMappingStr = struct 
  type t = [ `Cps of int list | `Self ]

  let property = Uucd.decomposition_mapping 
  let default = `Self
  let name = "DecompositionMapping"
  let type_name = "[`Cps of int list | `Self]"
end

module DecompositionMapping = ProcProp(DecompositionMappingStr)

let () = DecompositionMapping.run ()

(* decomposition_type *)

module DecompositionTypeStr = struct 
  type t = [ `Can
       | `Com
       | `Enc
       | `Fin
       | `Font
       | `Fra
       | `Init
       | `Iso
       | `Med
       | `Nar
       | `Nb
       | `None
       | `Sml
       | `Sqr
       | `Sub
       | `Sup
       | `Vert
       | `Wide ] 

  let property = Uucd.decomposition_type
  let default = `None
  let name = "DecompositionType"
  let type_name = "[ `Can
       | `Com
       | `Enc
       | `Fin
       | `Font
       | `Fra
       | `Init
       | `Iso
       | `Med
       | `Nar
       | `Nb
       | `None
       | `Sml
       | `Sqr
       | `Sub
       | `Sup
       | `Vert
       | `Wide ]"
end

module DecompositionType = ProcProp(DecompositionTypeStr)

let () = DecompositionType.run ()

(* default_ignorable_code_point *)

module DefaultIgnorableCodePointStr = struct
  let property = Uucd.default_ignorable_code_point
  let name = "DefaultIgnorableCodePoint"
end

module DefaultIgnorableCodePoint = ProcBoolProp(DefaultIgnorableCodePointStr)
let () = DefaultIgnorableCodePoint.run ()

(* deprecated *)

module DeprecatedStr = struct
  let property = Uucd.deprecated
  let name = "Deprecated"
end

module Deprecated = ProcBoolProp(DeprecatedStr)
let () = Deprecated.run ()

(* diacritic *)

module DiacriticStr = struct
  let property = Uucd.diacritic
  let name = "Diacritic"
end

module Diacritic = ProcBoolProp(DiacriticStr)
let () = Diacritic.run ()

(* east_asian_width *)

let is_private cp =
  cp >= 0xe000 && cp <= 0xf8ff 
|| cp >= 0xf0000 && cp <= 0xffffd
|| cp >= 0x100000 && cp <= 0x10fffd

module EastAsianWidthStr = struct 
  type t = [ `A | `F | `H | `N | `Na | `W ]  

  let property = Uucd.east_asian_width

  let default_value cp =
    if is_private cp then `A else
    if cp >= 0x4e00 && cp <= 0x9fff
    || cp >= 0x3400 && cp <= 0x4dbf
    || cp >= 0xf900 && cp <= 0xfaff
    || cp >= 0x20000 && cp <= 0x2ffff
    || cp >= 0x30000 && cp <= 0x3ffff then `W else `N      

  let name = "EastAsianWidth"
  let type_name = "[ `A | `F | `H | `N | `Na | `W ]"
end

module EastAsianWidth = ProcComplexProp(EastAsianWidthStr)
let () = EastAsianWidth.run ()

module ExpandsOnNFCStr = struct
  let property = Uucd.expands_on_nfc
  let name = "ExpandsOnNFC"
end

module ExpandsOnNFC = ProcBoolProp(ExpandsOnNFCStr)
let () = ExpandsOnNFC.run ()

module ExpandsOnNFDStr = struct
  let property = Uucd.expands_on_nfd
  let name = "ExpandsOnNFD"
end

module ExpandsOnNFD = ProcBoolProp(ExpandsOnNFDStr)
let () = ExpandsOnNFD.run ()

module ExpandsOnNFKCStr = struct
  let property = Uucd.expands_on_nfkc
  let name = "ExpandsOnNFKC"
end

module ExpandsOnNFKC = ProcBoolProp(ExpandsOnNFKCStr)
let () = ExpandsOnNFKC.run ()

module ExpandsOnNFKDStr = struct
  let property = Uucd.expands_on_nfkd
  let name = "ExpandsOnNFKD"
end

module ExpandsOnNFKD = ProcBoolProp(ExpandsOnNFKDStr)
let () = ExpandsOnNFKD.run ()

module ExtenderStr = struct
  let property = Uucd.extender
  let name = "Extender"
end

module Extender = ProcBoolProp(ExtenderStr)
let () = Extender.run ()
