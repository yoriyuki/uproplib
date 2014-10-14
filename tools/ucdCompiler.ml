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
  type t = [ `Cp of Uucd.cp | `Self ]

  let property = Uucd.bidi_paired_bracket
  let default = `Self
  let name = "BidiPairedBracket"
  let type_name = "[ `Cp of Uucd.cp | `Self ]"
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
  type t = [ `Cps of Uucd.cp list | `Self ]

  let property = Uucd.case_folding 
  let default = `Self
  let name = "CaseFolding"
  let type_name = "[`Cps of Uucd.cp list | `Self]"
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
  type t = [ `Cps of Uucd.cp list | `Self ]

  let property = Uucd.decomposition_mapping 
  let default = `Self
  let name = "DecompositionMapping"
  let type_name = "[`Cps of Uucd.cp list | `Self]"
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

module FcNFKCClosureStr = struct
  type t = [ `Cps of Uucd.cp list | `Self ]

  let property = Uucd.fc_nfkc_closure

  let default = `Self

  let name = "FcNFKCClosure"
  let type_name = "[ `Cps of Uucd.cp list | `Self ]"
end

module FcNFKCClosure = ProcProp(FcNFKCClosureStr)
let () = FcNFKCClosure.run ()

module FullCompositionExclusionStr = struct
  let property = Uucd.full_composition_exclusion
  let name = "FullCompositionExclusion"
end

module FullCompositionExclusion = ProcBoolProp(FullCompositionExclusionStr)
let () = FullCompositionExclusion.run ()

module GeneralCategoryStr = struct
  type t = [ `Cc
       | `Cf
       | `Cn
       | `Co
       | `Cs
       | `Ll
       | `Lm
       | `Lo
       | `Lt
       | `Lu
       | `Mc
       | `Me
       | `Mn
       | `Nd
       | `Nl
       | `No
       | `Pc
       | `Pd
       | `Pe
       | `Pf
       | `Pi
       | `Po
       | `Ps
       | `Sc
       | `Sk
       | `Sm
       | `So
       | `Zl
       | `Zp
       | `Zs ]

  let property = Uucd.general_category
  let default = `Cn

  let name = "GeneralCategory"
  let type_name = "[ `Cc
       | `Cf
       | `Cn
       | `Co
       | `Cs
       | `Ll
       | `Lm
       | `Lo
       | `Lt
       | `Lu
       | `Mc
       | `Me
       | `Mn
       | `Nd
       | `Nl
       | `No
       | `Pc
       | `Pd
       | `Pe
       | `Pf
       | `Pi
       | `Po
       | `Ps
       | `Sc
       | `Sk
       | `Sm
       | `So
       | `Zl
       | `Zp
       | `Zs ]"
end

module GeneralCategory = ProcProp(GeneralCategoryStr)
let () = GeneralCategory.run ()

module GraphemeBaseStr = struct
  let property = Uucd.grapheme_base
  let name = "GraphemeBase"
end

module GraphemeBase = ProcBoolProp(GraphemeBaseStr)
let () = GraphemeBase.run ()

module GraphemeClusterBreakStr = struct
  type t = [ `CN | `CR | `EX | `L | `LF | `LV | `LVT | `PP | `RI | `SM | `T | `V | `XX ]

  let property = Uucd.grapheme_cluster_break
  let default = `XX

  let name = "GraphemeClusterBreak"
  let type_name = "[ `CN | `CR | `EX | `L | `LF | `LV | `LVT | `PP | `RI | `SM | `T | `V | `XX ]"
end

module GraphemeClusterBreak = ProcProp(GraphemeClusterBreakStr)
let () = GraphemeClusterBreak.run ()

module GraphemeExtendStr = struct
  let property = Uucd.grapheme_extend
  let name = "GraphemeExtend"
end

module GraphemeExtend = ProcBoolProp(GraphemeExtendStr)
let () = GraphemeExtend.run ()

module GraphemeLinkStr = struct
  let property = Uucd.grapheme_link
  let name = "GraphemeLink"
end

module GraphemeLink = ProcBoolProp(GraphemeLinkStr)
let () = GraphemeLink.run ()

module HangulSyllableTypeStr = struct
  type t = [ `L | `LV | `LVT | `NA | `T | `V ]

  let property = Uucd.hangul_syllable_type
  let default = `NA

  let name = "HangulSyllableType"
  let type_name = "[ `L | `LV | `LVT | `NA | `T | `V ]"
end

module HangulSyllableType = ProcProp(HangulSyllableTypeStr)
let () = HangulSyllableType.run ()

module HexDigitStr = struct
  let property = Uucd.hex_digit
  let name = "HexDigit"
end

module HexDigit = ProcBoolProp(HexDigitStr)
let () = HexDigit.run ()

module HyphenStr = struct
  let property = Uucd.hyphen
  let name = "Hyphen"
end

module Hyphen = ProcBoolProp(HyphenStr)
let () = Hyphen.run ()

module IdContinueStr = struct
  let property = Uucd.id_continue
  let name = "IdContinue"
end

module IdContinue = ProcBoolProp(IdContinueStr)
let () = IdContinue.run ()

module IdStartStr = struct
  let property = Uucd.id_start
  let name = "IdStart"
end

module IdStart = ProcBoolProp(IdStartStr)
let () = IdStart.run ()

module IdeographicStr = struct
  let property = Uucd.ideographic
  let name = "Ideographic"
end

module Ideographic = ProcBoolProp(IdeographicStr)
let () = Ideographic.run ()

module IdsBinaryOperatorStr = struct
  let property = Uucd.ids_binary_operator
  let name = "IdsBinaryOperator"
end

module IdsBinaryOperator = ProcBoolProp(IdsBinaryOperatorStr)
let () = IdsBinaryOperator.run ()

module IdsTrinaryOperatorStr = struct
  let property = Uucd.ids_trinary_operator
  let name = "IdsTrinaryOperator"
end

module IdsTrinaryOperator = ProcBoolProp(IdsTrinaryOperatorStr)
let () = IdsTrinaryOperator.run ()

module IndicSyllabicCategoryStr = struct
  type t = [ `Avagraha
       | `Bindu
       | `Brahmi_Joining_Number
       | `Cantillation_Mark
       | `Consonant
       | `Consonant_Dead
       | `Consonant_Final
       | `Consonant_Head_Letter
       | `Consonant_Medial
       | `Consonant_Placeholder
       | `Consonant_Preceding_Repha
       | `Consonant_Repha
       | `Consonant_Subjoined
       | `Consonant_Succeeding_Repha
       | `Gemination_Mark
       | `Invisible_Stacker
       | `Joiner
       | `Modifying_Letter
       | `Non_Joiner
       | `Nukta
       | `Number
       | `Number_Joiner
       | `Other
       | `Pure_Killer
       | `Register_Shifter
       | `Tone_Letter
       | `Tone_Mark
       | `Virama
       | `Visarga
       | `Vowel
       | `Vowel_Dependent
       | `Vowel_Independent ]

  let property = Uucd.indic_syllabic_category
  let default = `Other

  let name = "IndicSyllabicCategory"
  let type_name = "[ `Avagraha
       | `Bindu
       | `Brahmi_Joining_Number
       | `Cantillation_Mark
       | `Consonant
       | `Consonant_Dead
       | `Consonant_Final
       | `Consonant_Head_Letter
       | `Consonant_Medial
       | `Consonant_Placeholder
       | `Consonant_Preceding_Repha
       | `Consonant_Repha
       | `Consonant_Subjoined
       | `Consonant_Succeeding_Repha
       | `Gemination_Mark
       | `Invisible_Stacker
       | `Joiner
       | `Modifying_Letter
       | `Non_Joiner
       | `Nukta
       | `Number
       | `Number_Joiner
       | `Other
       | `Pure_Killer
       | `Register_Shifter
       | `Tone_Letter
       | `Tone_Mark
       | `Virama
       | `Visarga
       | `Vowel
       | `Vowel_Dependent
       | `Vowel_Independent ]"
end

module IndicSyllabicCategory = ProcProp(IndicSyllabicCategoryStr)
let () = IndicSyllabicCategory.run ()

module IndicMatraCategoryStr = struct
  type t = [ `Bottom
       | `Bottom_And_Right
       | `Invisible
       | `Left
       | `Left_And_Right
       | `NA
       | `Overstruck
       | `Right
       | `Top
       | `Top_And_Bottom
       | `Top_And_Bottom_And_Right
       | `Top_And_Left
       | `Top_And_Left_And_Right
       | `Top_And_Right
       | `Visual_Order_Left ]

  let property = Uucd.indic_matra_category
  let default = `NA

  let name = "IndicMatraCategory"
  let type_name = "[ `Bottom
       | `Bottom_And_Right
       | `Invisible
       | `Left
       | `Left_And_Right
       | `NA
       | `Overstruck
       | `Right
       | `Top
       | `Top_And_Bottom
       | `Top_And_Bottom_And_Right
       | `Top_And_Left
       | `Top_And_Left_And_Right
       | `Top_And_Right
       | `Visual_Order_Left ]"
end

module IndicMatraCategory = ProcProp(IndicMatraCategoryStr)
let () = IndicMatraCategory.run ()

module IsoCommentStr = struct
  type t = string

  let property = Uucd.iso_comment
  let default = ""

  let name = "IsoComment"
  let type_name = "string"
end

module IsoComment = ProcProp(IsoCommentStr)
let () = IsoComment.run ()

module JamoShortNameStr = struct
  type t = string

  let property = Uucd.jamo_short_name
  let default = ""

  let name = "JamoShortName"
  let type_name = "string"
end

module JamoShortName = ProcProp(JamoShortNameStr)
let () = JamoShortName.run ()

module JoinControlStr = struct
  let property = Uucd.join_control
  let name = "JoinControl"
end

module JoinControl = ProcBoolProp(JoinControlStr)
let () = JoinControl.run ()

module JoiningGroupStr = struct
  type t = [ `Ain
       | `Alaph
       | `Alef
       | `Alef_Maqsurah
       | `Beh
       | `Beth
       | `Burushaski_Yeh_Barree
       | `Dal
       | `Dalath_Rish
       | `E
       | `Farsi_Yeh
       | `Fe
       | `Feh
       | `Final_Semkath
       | `Gaf
       | `Gamal
       | `Hah
       | `Hamza_On_Heh_Goal
       | `He
       | `Heh
       | `Heh_Goal
       | `Heth
       | `Kaf
       | `Kaph
       | `Khaph
       | `Knotted_Heh
       | `Lam
       | `Lamadh
       | `Manichaean_Aleph
       | `Manichaean_Ayin
       | `Manichaean_Beth
       | `Manichaean_Daleth
       | `Manichaean_Dhamedh
       | `Manichaean_Five
       | `Manichaean_Gimel
       | `Manichaean_Heth
       | `Manichaean_Hundred
       | `Manichaean_Kaph
       | `Manichaean_Lamedh
       | `Manichaean_Mem
       | `Manichaean_Nun
       | `Manichaean_One
       | `Manichaean_Pe
       | `Manichaean_Qoph
       | `Manichaean_Resh
       | `Manichaean_Sadhe
       | `Manichaean_Samekh
       | `Manichaean_Taw
       | `Manichaean_Ten
       | `Manichaean_Teth
       | `Manichaean_Thamedh
       | `Manichaean_Twenty
       | `Manichaean_Waw
       | `Manichaean_Yodh
       | `Manichaean_Zayin
       | `Meem
       | `Mim
       | `No_Joining_Group
       | `Noon
       | `Nun
       | `Nya
       | `Pe
       | `Qaf
       | `Qaph
       | `Reh
       | `Reversed_Pe
       | `Rohingya_Yeh
       | `Sad
       | `Sadhe
       | `Seen
       | `Semkath
       | `Shin
       | `Straight_Waw
       | `Swash_Kaf
       | `Syriac_Waw
       | `Tah
       | `Taw
       | `Teh_Marbuta
       | `Teh_Marbuta_Goal
       | `Teth
       | `Waw
       | `Yeh
       | `Yeh_Barree
       | `Yeh_With_Tail
       | `Yudh
       | `Yudh_He
       | `Zain
       | `Zhain ]

  let property = Uucd.joining_group
  let default = `No_Joining_Group

  let name = "JoiningGroup"
      let type_name = "[ `Ain
       | `Alaph
       | `Alef
       | `Alef_Maqsurah
       | `Beh
       | `Beth
       | `Burushaski_Yeh_Barree
       | `Dal
       | `Dalath_Rish
       | `E
       | `Farsi_Yeh
       | `Fe
       | `Feh
       | `Final_Semkath
       | `Gaf
       | `Gamal
       | `Hah
       | `Hamza_On_Heh_Goal
       | `He
       | `Heh
       | `Heh_Goal
       | `Heth
       | `Kaf
       | `Kaph
       | `Khaph
       | `Knotted_Heh
       | `Lam
       | `Lamadh
       | `Manichaean_Aleph
       | `Manichaean_Ayin
       | `Manichaean_Beth
       | `Manichaean_Daleth
       | `Manichaean_Dhamedh
       | `Manichaean_Five
       | `Manichaean_Gimel
       | `Manichaean_Heth
       | `Manichaean_Hundred
       | `Manichaean_Kaph
       | `Manichaean_Lamedh
       | `Manichaean_Mem
       | `Manichaean_Nun
       | `Manichaean_One
       | `Manichaean_Pe
       | `Manichaean_Qoph
       | `Manichaean_Resh
       | `Manichaean_Sadhe
       | `Manichaean_Samekh
       | `Manichaean_Taw
       | `Manichaean_Ten
       | `Manichaean_Teth
       | `Manichaean_Thamedh
       | `Manichaean_Twenty
       | `Manichaean_Waw
       | `Manichaean_Yodh
       | `Manichaean_Zayin
       | `Meem
       | `Mim
       | `No_Joining_Group
       | `Noon
       | `Nun
       | `Nya
       | `Pe
       | `Qaf
       | `Qaph
       | `Reh
       | `Reversed_Pe
       | `Rohingya_Yeh
       | `Sad
       | `Sadhe
       | `Seen
       | `Semkath
       | `Shin
       | `Straight_Waw
       | `Swash_Kaf
       | `Syriac_Waw
       | `Tah
       | `Taw
       | `Teh_Marbuta
       | `Teh_Marbuta_Goal
       | `Teth
       | `Waw
       | `Yeh
       | `Yeh_Barree
       | `Yeh_With_Tail
       | `Yudh
       | `Yudh_He
       | `Zain
       | `Zhain ]"
end

module JoiningGroup = ProcProp(JoiningGroupStr)
let () = JoiningGroup.run ()

module JoiningTypeStr = struct
  type t = [ `C | `D | `L | `R | `T | `U ]

  let property = Uucd.joining_type
  let default = `U

  let name = "JoiningType"
  let type_name = "[ `C | `D | `L | `R | `T | `U | `Non_Joining]"
end

module JoiningType = ProcProp(JoiningTypeStr)
let () = JoiningType.run ()

module LineBreakStr = struct
  type t = [ `AI
       | `AL
       | `B2
       | `BA
       | `BB
       | `BK
       | `CB
       | `CJ
       | `CL
       | `CM
       | `CP
       | `CR
       | `EX
       | `GL
       | `H2
       | `H3
       | `HL
       | `HY
       | `ID
       | `IN
       | `IS
       | `JL
       | `JT
       | `JV
       | `LF
       | `NL
       | `NS
       | `NU
       | `OP
       | `PO
       | `PR
       | `QU
       | `RI
       | `SA
       | `SG
       | `SP
       | `SY
       | `WJ
       | `XX
       | `ZW ]

  let property = Uucd.line_break
  let default = `XX

  let name = "LineBreak"
  let type_name =  "[ `AI
       | `AL
       | `B2
       | `BA
       | `BB
       | `BK
       | `CB
       | `CJ
       | `CL
       | `CM
       | `CP
       | `CR
       | `EX
       | `GL
       | `H2
       | `H3
       | `HL
       | `HY
       | `ID
       | `IN
       | `IS
       | `JL
       | `JT
       | `JV
       | `LF
       | `NL
       | `NS
       | `NU
       | `OP
       | `PO
       | `PR
       | `QU
       | `RI
       | `SA
       | `SG
       | `SP
       | `SY
       | `WJ
       | `XX
       | `ZW ]"
end

module LineBreak = ProcProp(LineBreakStr)
let () = LineBreak.run ()

module LogicalOrderExceptionStr = struct
  let property = Uucd.logical_order_exception
  let name = "LogicalOrderException"
end

module LogicalOrderException = ProcBoolProp(LogicalOrderExceptionStr)
let () = LogicalOrderException.run ()

module LowercaseStr = struct
  let property = Uucd.lowercase
  let name = "Lowercase"
end

module Lowercase = ProcBoolProp(LowercaseStr)
let () = Lowercase.run ()

module LowercaseMappingStr = struct 
  type t = [ `Cps of int list | `Self ]
  let property = Uucd.lowercase_mapping
  let default = `Self
  let name = "LowercaseMapping"
  let type_name = "[ `Cps of int list | `Self ]"
end

module LowercaseMapping = ProcProp(LowercaseMappingStr)
let () = LowercaseMapping.run ()

module MathStr = struct
  let property = Uucd.math
  let name = "Math"
end

module Math = ProcBoolProp(MathStr)
let () = Math.run ()

module NameStr = struct
  type t = [ `Name of string | `Pattern of string ]
  let property = Uucd.name
  let default = `Name ""
  let name = "Name"
  let type_name = "[ `Name of string | `Pattern of string ]"
end

module Name = ProcProp(NameStr)
let () = Name.run ()

module NameAliasesStr = struct
  type t =  
      (string * 
	 [ `Abbreviation | `Alternate | `Control | `Correction | `Figment ]) list
  let property = Uucd.name_alias
  let default = []
  let name = "NameAliases"
  let type_name = "(string * 
      [ `Abbreviation | `Alternate | `Control | `Correction | `Figment ]) list"
end

module NameAliases = ProcProp(NameAliasesStr)
let () = NameAliases.run ()

module NFCQuickCheckStr = struct 
  type t = [ `False | `Maybe | `True ]
  let property = Uucd.nfc_quick_check
  let default = `Maybe

  let name = "NFCQuickCheck"
  let type_name = "[ `False | `Maybe | `True ]"
end

module NFCQuickCheck = ProcProp(NFCQuickCheckStr)
let () = NFCQuickCheck.run ()

module NFDQuickCheckStr = struct 
  type t = [ `False | `Maybe | `True ]
  let property = Uucd.nfd_quick_check
  let default = `Maybe

  let name = "NFDQuickCheck"
  let type_name = "[ `False | `Maybe | `True ]"
end

module NFDQuickCheck = ProcProp(NFDQuickCheckStr)
let () = NFDQuickCheck.run ()

module NFKCQuickCheckStr = struct 
  type t = [ `False | `Maybe | `True ]
  let property = Uucd.nfkc_quick_check
  let default = `Maybe

  let name = "NFKCQuickCheck"
  let type_name = "[ `False | `Maybe | `True ]"
end

module NFKCQuickCheck = ProcProp(NFKCQuickCheckStr)
let () = NFKCQuickCheck.run ()

module NFKDQuickCheckStr = struct 
  type t = [ `False | `Maybe | `True ]
  let property = Uucd.nfkd_quick_check
  let default = `Maybe

  let name = "NFKDQuickCheck"
  let type_name = "[ `False | `Maybe | `True ]"
end

module NFKDQuickCheck = ProcProp(NFKDQuickCheckStr)
let () = NFKDQuickCheck.run ()

module NFKCCasefoldStr = struct
  type t = [ `Cps of int list | `Self ]
  let property = Uucd.nfkc_casefold
  let default = `Self

  let name = "NFKCCasefold"
  let type_name = "[ `Cps of int list | `Self ]"
end

module NFKCCasefold = ProcProp(NFKCCasefoldStr)
let () = NFKCCasefold.run ()

module NoncharacterCodePointStr = struct
  let property = Uucd.noncharacter_code_point
  let name = "NoncharacterCodePoint"
end

module NoncharacterCodePoint = ProcBoolProp(NoncharacterCodePointStr)
let () = NoncharacterCodePoint.run ()

module NumericTypeStr = struct
  type t = [ `De | `Di | `None | `Nu ]
  let property = Uucd.numeric_type
  let default = `None
  let name = "NumericType"
  let type_name = "[ `De | `Di | `None | `Nu ]"
end

module NumericType = ProcProp(NumericTypeStr)
let () = NumericType.run ()

module NumericValueStr = struct
  type t = [ `Frac of int * int | `NaN | `Num of int64 ]
  let property = Uucd.numeric_value
  let default = `NaN
  let name = "NumericValue"
let type_name = "[ `Frac of int * int | `NaN | `Num of int64 ]"
end

module NumericValue = ProcProp(NumericValueStr)
let () = NumericValue.run ()


module OtherAlphabeticStr = struct
  let property = Uucd.other_alphabetic
  let name = "OtherAlphabetic"
end
module OtherAlphabetic = ProcBoolProp(OtherAlphabeticStr)
let () = OtherAlphabetic.run ()

module OtherDefaultIgnorableCodePointStr = struct
  let property = Uucd.other_default_ignorable_code_point
  let name = "OtherDefaultIgnorableCodePoint"
end
module OtherDefaultIgnorableCodePoint = ProcBoolProp(OtherDefaultIgnorableCodePointStr)
let () = OtherDefaultIgnorableCodePoint.run ()

module OtherGraphemeExtendStr = struct
  let property = Uucd.other_grapheme_extend
  let name = "OtherGraphemeExtend"
end
module OtherGraphemeExtend = ProcBoolProp(OtherGraphemeExtendStr)
let () = OtherGraphemeExtend.run ()

module OtherIdContinueStr = struct
  let property = Uucd.other_id_continue
  let name = "OtherIdContinue"
end
module OtherIdContinue = ProcBoolProp(OtherIdContinueStr)
let () = OtherIdContinue.run ()

module OtherIdStartStr = struct
  let property = Uucd.other_id_start
  let name = "OtherIdStart"
end
module OtherIdStart = ProcBoolProp(OtherIdStartStr)
let () = OtherIdStart.run ()

module OtherLowercaseStr = struct
  let property = Uucd.other_lowercase
  let name = "OtherLowercase"
end
module OtherLowercase = ProcBoolProp(OtherLowercaseStr)
let () = OtherLowercase.run ()

module OtherMathStr = struct
  let property = Uucd.other_math
  let name = "OtherMath"
end
module OtherMath = ProcBoolProp(OtherMathStr)
let () = OtherMath.run ()

module OtherUppercaseStr = struct
  let property = Uucd.other_uppercase
  let name = "OtherUppercase"
end
module OtherUppercase = ProcBoolProp(OtherUppercaseStr)
let () = OtherUppercase.run ()

module PatternSyntaxStr = struct
  let property = Uucd.pattern_syntax
  let name = "PatternSyntax"
end
module PatternSyntax = ProcBoolProp(PatternSyntaxStr)
let () = PatternSyntax.run ()

module PatternWhiteSpaceStr = struct
  let property = Uucd.pattern_white_space
  let name = "PatternWhiteSpace"
end
module PatternWhiteSpace = ProcBoolProp(PatternWhiteSpaceStr)
let () = PatternWhiteSpace.run ()

module QuotationMarkStr = struct
  let property = Uucd.quotation_mark
  let name = "QuotationMark"
end
module QuotationMark = ProcBoolProp(QuotationMarkStr)
let () = QuotationMark.run ()

module RadicalStr = struct
  let property = Uucd.radical
  let name = "Radical"
end
module Radical = ProcBoolProp(RadicalStr)
let () = Radical.run ()

module ScriptStr = struct
  type t = [ `Aghb
       | `Arab
       | `Armi
       | `Armn
       | `Avst
       | `Bali
       | `Bamu
       | `Bass
       | `Batk
       | `Beng
       | `Bopo
       | `Brah
       | `Brai
       | `Bugi
       | `Buhd
       | `Cakm
       | `Cans
       | `Cari
       | `Cham
       | `Cher
       | `Copt
       | `Cprt
       | `Cyrl
       | `Deva
       | `Dsrt
       | `Dupl
       | `Egyp
       | `Elba
       | `Ethi
       | `Geor
       | `Glag
       | `Goth
       | `Gran
       | `Grek
       | `Gujr
       | `Guru
       | `Hang
       | `Hani
       | `Hano
       | `Hebr
       | `Hira
       | `Hmng
       | `Hrkt
       | `Ital
       | `Java
       | `Kali
       | `Kana
       | `Khar
       | `Khmr
       | `Khoj
       | `Knda
       | `Kthi
       | `Lana
       | `Laoo
       | `Latn
       | `Lepc
       | `Limb
       | `Lina
       | `Linb
       | `Lisu
       | `Lyci
       | `Lydi
       | `Mahj
       | `Mand
       | `Mani
       | `Mend
       | `Merc
       | `Mero
       | `Mlym
       | `Modi
       | `Mong
       | `Mroo
       | `Mtei
       | `Mymr
       | `Narb
       | `Nbat
       | `Nkoo
       | `Ogam
       | `Olck
       | `Orkh
       | `Orya
       | `Osma
       | `Palm
       | `Pauc
       | `Perm
       | `Phag
       | `Phli
       | `Phlp
       | `Phnx
       | `Plrd
       | `Prti
       | `Qaai
       | `Rjng
       | `Runr
       | `Samr
       | `Sarb
       | `Saur
       | `Shaw
       | `Shrd
       | `Sidd
       | `Sind
       | `Sinh
       | `Sora
       | `Sund
       | `Sylo
       | `Syrc
       | `Tagb
       | `Takr
       | `Tale
       | `Talu
       | `Taml
       | `Tavt
       | `Telu
       | `Tfng
       | `Tglg
       | `Thaa
       | `Thai
       | `Tibt
       | `Tirh
       | `Ugar
       | `Vaii
       | `Wara
       | `Xpeo
       | `Xsux
       | `Yiii
       | `Zinh
       | `Zyyy
       | `Zzzz ] 
  let property = Uucd.script
  let default = `Zzzz
  let name = "Script"
  let type_name = "[ `Aghb
       | `Arab
       | `Armi
       | `Armn
       | `Avst
       | `Bali
       | `Bamu
       | `Bass
       | `Batk
       | `Beng
       | `Bopo
       | `Brah
       | `Brai
       | `Bugi
       | `Buhd
       | `Cakm
       | `Cans
       | `Cari
       | `Cham
       | `Cher
       | `Copt
       | `Cprt
       | `Cyrl
       | `Deva
       | `Dsrt
       | `Dupl
       | `Egyp
       | `Elba
       | `Ethi
       | `Geor
       | `Glag
       | `Goth
       | `Gran
       | `Grek
       | `Gujr
       | `Guru
       | `Hang
       | `Hani
       | `Hano
       | `Hebr
       | `Hira
       | `Hmng
       | `Hrkt
       | `Ital
       | `Java
       | `Kali
       | `Kana
       | `Khar
       | `Khmr
       | `Khoj
       | `Knda
       | `Kthi
       | `Lana
       | `Laoo
       | `Latn
       | `Lepc
       | `Limb
       | `Lina
       | `Linb
       | `Lisu
       | `Lyci
       | `Lydi
       | `Mahj
       | `Mand
       | `Mani
       | `Mend
       | `Merc
       | `Mero
       | `Mlym
       | `Modi
       | `Mong
       | `Mroo
       | `Mtei
       | `Mymr
       | `Narb
       | `Nbat
       | `Nkoo
       | `Ogam
       | `Olck
       | `Orkh
       | `Orya
       | `Osma
       | `Palm
       | `Pauc
       | `Perm
       | `Phag
       | `Phli
       | `Phlp
       | `Phnx
       | `Plrd
       | `Prti
       | `Qaai
       | `Rjng
       | `Runr
       | `Samr
       | `Sarb
       | `Saur
       | `Shaw
       | `Shrd
       | `Sidd
       | `Sind
       | `Sinh
       | `Sora
       | `Sund
       | `Sylo
       | `Syrc
       | `Tagb
       | `Takr
       | `Tale
       | `Talu
       | `Taml
       | `Tavt
       | `Telu
       | `Tfng
       | `Tglg
       | `Thaa
       | `Thai
       | `Tibt
       | `Tirh
       | `Ugar
       | `Vaii
       | `Wara
       | `Xpeo
       | `Xsux
       | `Yiii
       | `Zinh
       | `Zyyy
       | `Zzzz ]"
end
module Script = ProcProp(ScriptStr)
let () = Script.run ()

module ScriptExtensionsStr = struct
  type t = [ `Aghb
       | `Arab
       | `Armi
       | `Armn
       | `Avst
       | `Bali
       | `Bamu
       | `Bass
       | `Batk
       | `Beng
       | `Bopo
       | `Brah
       | `Brai
       | `Bugi
       | `Buhd
       | `Cakm
       | `Cans
       | `Cari
       | `Cham
       | `Cher
       | `Copt
       | `Cprt
       | `Cyrl
       | `Deva
       | `Dsrt
       | `Dupl
       | `Egyp
       | `Elba
       | `Ethi
       | `Geor
       | `Glag
       | `Goth
       | `Gran
       | `Grek
       | `Gujr
       | `Guru
       | `Hang
       | `Hani
       | `Hano
       | `Hebr
       | `Hira
       | `Hmng
       | `Hrkt
       | `Ital
       | `Java
       | `Kali
       | `Kana
       | `Khar
       | `Khmr
       | `Khoj
       | `Knda
       | `Kthi
       | `Lana
       | `Laoo
       | `Latn
       | `Lepc
       | `Limb
       | `Lina
       | `Linb
       | `Lisu
       | `Lyci
       | `Lydi
       | `Mahj
       | `Mand
       | `Mani
       | `Mend
       | `Merc
       | `Mero
       | `Mlym
       | `Modi
       | `Mong
       | `Mroo
       | `Mtei
       | `Mymr
       | `Narb
       | `Nbat
       | `Nkoo
       | `Ogam
       | `Olck
       | `Orkh
       | `Orya
       | `Osma
       | `Palm
       | `Pauc
       | `Perm
       | `Phag
       | `Phli
       | `Phlp
       | `Phnx
       | `Plrd
       | `Prti
       | `Qaai
       | `Rjng
       | `Runr
       | `Samr
       | `Sarb
       | `Saur
       | `Shaw
       | `Shrd
       | `Sidd
       | `Sind
       | `Sinh
       | `Sora
       | `Sund
       | `Sylo
       | `Syrc
       | `Tagb
       | `Takr
       | `Tale
       | `Talu
       | `Taml
       | `Tavt
       | `Telu
       | `Tfng
       | `Tglg
       | `Thaa
       | `Thai
       | `Tibt
       | `Tirh
       | `Ugar
       | `Vaii
       | `Wara
       | `Xpeo
       | `Xsux
       | `Yiii
       | `Zinh
       | `Zyyy
       | `Zzzz ] list
  let property = Uucd.script_extensions
  let default = []
  let name = "ScriptExtensions"
  let type_name = "[ `Aghb
       | `Arab
       | `Armi
       | `Armn
       | `Avst
       | `Bali
       | `Bamu
       | `Bass
       | `Batk
       | `Beng
       | `Bopo
       | `Brah
       | `Brai
       | `Bugi
       | `Buhd
       | `Cakm
       | `Cans
       | `Cari
       | `Cham
       | `Cher
       | `Copt
       | `Cprt
       | `Cyrl
       | `Deva
       | `Dsrt
       | `Dupl
       | `Egyp
       | `Elba
       | `Ethi
       | `Geor
       | `Glag
       | `Goth
       | `Gran
       | `Grek
       | `Gujr
       | `Guru
       | `Hang
       | `Hani
       | `Hano
       | `Hebr
       | `Hira
       | `Hmng
       | `Hrkt
       | `Ital
       | `Java
       | `Kali
       | `Kana
       | `Khar
       | `Khmr
       | `Khoj
       | `Knda
       | `Kthi
       | `Lana
       | `Laoo
       | `Latn
       | `Lepc
       | `Limb
       | `Lina
       | `Linb
       | `Lisu
       | `Lyci
       | `Lydi
       | `Mahj
       | `Mand
       | `Mani
       | `Mend
       | `Merc
       | `Mero
       | `Mlym
       | `Modi
       | `Mong
       | `Mroo
       | `Mtei
       | `Mymr
       | `Narb
       | `Nbat
       | `Nkoo
       | `Ogam
       | `Olck
       | `Orkh
       | `Orya
       | `Osma
       | `Palm
       | `Pauc
       | `Perm
       | `Phag
       | `Phli
       | `Phlp
       | `Phnx
       | `Plrd
       | `Prti
       | `Qaai
       | `Rjng
       | `Runr
       | `Samr
       | `Sarb
       | `Saur
       | `Shaw
       | `Shrd
       | `Sidd
       | `Sind
       | `Sinh
       | `Sora
       | `Sund
       | `Sylo
       | `Syrc
       | `Tagb
       | `Takr
       | `Tale
       | `Talu
       | `Taml
       | `Tavt
       | `Telu
       | `Tfng
       | `Tglg
       | `Thaa
       | `Thai
       | `Tibt
       | `Tirh
       | `Ugar
       | `Vaii
       | `Wara
       | `Xpeo
       | `Xsux
       | `Yiii
       | `Zinh
       | `Zyyy
       | `Zzzz ] list"
end
module ScriptExtensions = ProcProp(ScriptExtensionsStr)
let () = ScriptExtensions.run ()

module SentenceBreakStr = struct
  type t = [ `AT
       | `CL
       | `CR
       | `EX
       | `FO
       | `LE
       | `LF
       | `LO
       | `NU
       | `SC
       | `SE
       | `SP
       | `ST
       | `UP
       | `XX ]
  let property = Uucd.sentence_break
  let default = `XX
  let name = "SentenceBreak"
  let type_name = "[ `AT
       | `CL
       | `CR
       | `EX
       | `FO
       | `LE
       | `LF
       | `LO
       | `NU
       | `SC
       | `SE
       | `SP
       | `ST
       | `UP
       | `XX ]"
end
module SentenceBreak = ProcProp(SentenceBreakStr)
let () = SentenceBreak.run ()


module SimpleCaseFoldingStr = struct
  type t = [ `Cp of int | `Self ]
  let property = Uucd.simple_case_folding
  let default = `Self
  let name = "SimpleCaseFolding"
  let type_name = "[ `Cp of int | `Self ]"
end
module SimpleCaseFolding = ProcProp(SimpleCaseFoldingStr)
let () = SimpleCaseFolding.run ()

module SimpleLowercaseMappingStr = struct
  type t = [ `Cp of int | `Self ]
  let property = Uucd.simple_lowercase_mapping
  let default = `Self
  let name = "SimpleLowercaseMapping"
  let type_name = "[ `Cp of int | `Self ]"
end
module SimpleLowercaseMapping = ProcProp(SimpleLowercaseMappingStr)
let () = SimpleLowercaseMapping.run ()

module SimpleTitlecaseMappingStr = struct
  type t = [ `Cp of int | `Self ]
  let property = Uucd.simple_titlecase_mapping
  let default = `Self
  let name = "SimpleTitlecaseMapping"
  let type_name = "[ `Cp of int | `Self ]"
end
module SimpleTitlecaseMapping = ProcProp(SimpleTitlecaseMappingStr)
let () = SimpleTitlecaseMapping.run ()

module SimpleUppercaseMappingStr = struct
  type t = [ `Cp of int | `Self ]
  let property = Uucd.simple_uppercase_mapping
  let default = `Self
  let name = "SimpleUppercaseMapping"
  let type_name = "[ `Cp of int | `Self ]"
end
module SimpleUppercaseMapping = ProcProp(SimpleUppercaseMappingStr)
let () = SimpleUppercaseMapping.run ()

module SoftDottedStr = struct
  let property = Uucd.soft_dotted
  let name = "SoftDotted"
end
module SoftDotted = ProcBoolProp(SoftDottedStr)
let () = SoftDotted.run ()

module StermStr = struct
  let property = Uucd.sterm
  let name = "Sterm"
end
module Sterm = ProcBoolProp(StermStr)
let () = Sterm.run ()

module TerminalPunctuationStr = struct
  let property = Uucd.terminal_punctuation
  let name = "TerminalPunctuation"
end
module TerminalPunctuation = ProcBoolProp(TerminalPunctuationStr)
let () = TerminalPunctuation.run ()

module TitlecaseMappingStr = struct
  type t = [ `Cps of int list | `Self ]
  let property = Uucd.titlecase_mapping
  let default = `Self
  let name = "TitlecaseMapping"
  let type_name = "[ `Cps of int list | `Self ]"
end
module TitlecaseMapping = ProcProp(TitlecaseMappingStr)
let () = TitlecaseMapping.run ()

module Unicode1NameStr = struct
  type t = string
  let property = Uucd.unicode_1_name
  let default = ""
  let name = "Unicode1Name"
  let type_name = "string"
end
module Unicode1Name = ProcProp(Unicode1NameStr)
let () = Unicode1Name.run ()

module UnifiedIdeographStr = struct
  let property = Uucd.unified_ideograph
  let name = "UnifiedIdeograph"
end
module UnifiedIdeograph = ProcBoolProp(UnifiedIdeographStr)
let () = UnifiedIdeograph.run ()

module UppercaseStr = struct
  let property = Uucd.uppercase
  let name = "Uppercase"
end
module Uppercase = ProcBoolProp(UppercaseStr)
let () = Uppercase.run ()

module UppercaseMappingStr = struct
  type t = [ `Cps of int list | `Self ]
  let property = Uucd.uppercase_mapping
  let default = `Self
  let name = "UppercaseMapping"
  let type_name = "[ `Cps of int list | `Self ]"
end
module UppercaseMapping = ProcProp(UppercaseMappingStr)
let () = UppercaseMapping.run ()

module VariationSelectorStr = struct
  let property = Uucd.variation_selector
  let name = "VariationSelector"
end
module VariationSelector = ProcBoolProp(VariationSelectorStr)
let () = VariationSelector.run ()

module WhiteSpaceStr = struct
  let property = Uucd.white_space
  let name = "WhiteSpace"
end
module WhiteSpace = ProcBoolProp(WhiteSpaceStr)
let () = WhiteSpace.run ()

module WordBreakStr = struct
  type t = [ `CR
       | `DQ
       | `EX
       | `Extend
       | `FO
       | `HL
       | `KA
       | `LE
       | `LF
       | `MB
       | `ML
       | `MN
       | `NL
       | `NU
       | `RI
       | `SQ
       | `XX ]
  let property = Uucd.word_break
  let default = `XX
  let name = "WordBreak"
  let type_name = "[ `CR
       | `DQ
       | `EX
       | `Extend
       | `FO
       | `HL
       | `KA
       | `LE
       | `LF
       | `MB
       | `ML
       | `MN
       | `NL
       | `NU
       | `RI
       | `SQ
       | `XX ]"
end
module WordBreak = ProcProp(WordBreakStr)
let () = WordBreak.run ()

module XidContinueStr = struct
  let property = Uucd.xid_continue
  let name = "XidContinue"
end
module XidContinue = ProcBoolProp(XidContinueStr)
let () = XidContinue.run ()

module XidStartStr = struct
  let property = Uucd.xid_start
  let name = "XidStart"
end
module XidStart = ProcBoolProp(XidStartStr)
let () = XidStart.run ()
