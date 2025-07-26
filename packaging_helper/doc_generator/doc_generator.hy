
; Imports ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (import  pyparsing :as pp)
    (import  _fptk_local *)
    (require _fptk_local *)

; _____________________________________________________________________________/ }}}1

    ; PARSER:

; CLASSES ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    ; used for creating DEFINED_FUNC as parent module name
    (setv $FPTK_NAME "fptk")

    ; "FPTK Group from VimCells"
    (defclass [dataclass] FGroup []
        (#^ str name)
        (#^ str raw_content))

    (defclass FEntityKind [Enum]
        (setv IMPORT_MODULE         0)
        (setv IMPORT_FROM_MODULE    1)
        (setv IMPORT_FROM_MODULE_AS 2)
        (setv REQUIRE_MACRO         3)
        (setv NON_IMPORT_INFO       4)
        (setv DEFINED_SETV          5)
        (setv DEFINED_FUNC          6))

    ; "FPTK Entity"
    (defclass [dataclass] FEntity []
        (#^ FEntityKind kind            #_ "IMPORT_FROM_MODULE")
        (#^ str         kind_str        #_ "used only for non-import-info: macro/func/...")
        (#^ str         org_name        #_ "used only for :as imports")
        (#^ str         name            #_ "rpartial")
        (#^ str         parent_module   #_ "funcy")
        (#^ str         signature       #_ "rpartial(f, *args)")
        (#^ str         descr           #_ "partially applicates"))

; _____________________________________________________________________________/ }}}1
; hy atoms ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (setv ALPHAS    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
    (setv WSYMBOLS  (+ "_" "$.-=+&*<>!/|:" "%^?"))
    (setv MSYMBOLS  "#`'~@\\,")
    (setv NUMS      "0123456789")

    (setv LPAR      (+ (pp.Optional "#") (pp.Literal "(")))
    (setv RPAR      (pp.Literal ")"))

    (setv LBRCKT    (pp.Literal "["))
    (setv RBRCKT    (pp.Literal "]"))

    (setv LCRB      (+ (pp.Optional "#") (pp.Literal "{")))
    (setv RCRB      (pp.Literal "}"))

    ; =========================================================

    (setv NUMBER (| (pp.Combine (+ (pp.Optional "-")
                                   (pp.Word NUMS)
                                   (pp.Optional ".")
                                   (pp.Optional (pp.Word NUMS))
                                   (pp.Optional (+ (pp.oneOf "e E")
                                                   (pp.Optional (pp.oneOf "- +"))
                                                   (pp.Word NUMS)))))
                    (pp.Combine (+ (pp.Word ".") (pp.Word NUMS)))))

    (setv UNPACKER     (| (pp.Literal "#**") (pp.Literal "#*")))
    (setv WORD         (pp.Word (+ ALPHAS WSYMBOLS) (+ ALPHAS NUMS WSYMBOLS ":")))
    (setv KEYWORD      (pp.Combine (+ ":" (pp.Word (+ ALPHAS "_") (+ ALPHAS "_" NUMS)))))
    (setv QSTRING      (pp.Combine (+  (pp.Optional (pp.oneOf "r f b"))
                                       (pp.QuotedString   :quoteChar "\""
                                                          :escChar "\\"
                                                          :multiline True
                                                          :unquoteResults False))))
    (setv OCOMMENT     (pp.Combine (+  (pp.Literal ";;")
                                       (pp.SkipTo (pp.lineEnd)))))

    ; ==========================
    ; ATOM    = words and similar
    ; EXPR    = bracketed
    ; CONTENT = 0+ words or bracketed

    (setv ICOMMENT     (pp.Forward))
    (setv ANNOTATION   (pp.Forward))
    (setv QEXPR        (pp.Forward))           ; [ ... ]
    (setv SEXPR        (pp.Forward))           ; ( ... ) #( ... )
    (setv CEXPR        (pp.Forward))           ; { ... } #{ ... }

    (setv ATOM         (|  OCOMMENT
                           ICOMMENT
                           ANNOTATION
                           QSTRING
                           KEYWORD
                           WORD
                           UNPACKER
                           NUMBER
                           (pp.oneOf MSYMBOLS)))

    (setv EXPR         (| QEXPR SEXPR CEXPR))

    (setv CONTENT      (pp.Group (pp.ZeroOrMore (| EXPR ATOM))))
    (<<   QEXPR        (pp.Group (+ LBRCKT CONTENT RBRCKT)))
    (<<   SEXPR        (pp.Group (+ LPAR   CONTENT RPAR)))
    (<<   CEXPR        (pp.Group (+ LCRB   CONTENT RCRB)))

    (<<   ICOMMENT     (pp.Group (+ (pp.Literal "#_") (| EXPR ATOM))))
    (<<   ANNOTATION   (pp.Group (+ (pp.Literal "#^") (| EXPR ATOM))))

; _____________________________________________________________________________/ }}}1
; strings utils ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    ; «"pups"» -> «pups»
    (defn #^ str remove_quotes [#^ str input_string]
        (setv string input_string)
        (when (= "\"" (first string)) (setv string (drop  1 string)))
        (when (= "\"" (last  string)) (setv string (drop -1 string)))
        string)

    (defn #^ str
        pad_string
        [ #^ str  string
          #^ int  required_len
          #^ str  [fill_char " "]
          #^ bool [pad_right False]
        ]
        "returns string with len >= required_len"
        (setv n_required (max 0 (- required_len (len string))))
		(if (= pad_right False)
			(setv outp (sconcat string (* fill_char n_required)))
			(setv outp (sconcat (* fill_char n_required) string)))
		(return outp))


; _____________________________________________________________________________/ }}}1

; split descr ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    ; [t]oken [o]riginal [r]esult [n]ame
    (defn t_orn [name token] (-> token pp.originalTextFor (.setResultsName name)))

    (setv $SEPARATOR "|")

    ; for normal:          «f(xs) | smth» -> ("f(xs)", "smth")
    ; for non-import-info: «module | kind | name | signature | descr»
    (defn #^ FEntity split_descr [#^ FEntity fentity]
        (setv org_descr fentity.descr)
        (setv result (lmap strip (org_descr.split $SEPARATOR)))
        ; normal case:
        (when (= (len result) 2)
              (setv fentity.signature (first  result))
              (setv fentity.descr     (second result)))
        ; non-import-info case:
        (when (= (len result) 5)
              (setv fentity (FEntity :kind          FEntityKind.NON_IMPORT_INFO
                                     :kind_str      (second result)
                                     :org_name      ""
                                     :name          (third  result)
                                     :parent_module (first  result)
                                     :signature     (fourth result)
                                     :descr         (nth  4 result))))
        (return fentity))

; _____________________________________________________________________________/ }}}1
; find groups (defined by VimCells) ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (setv FPTK_GROUP (+ "; [GROUP]" (t_orn "_NAME" (pp.OneOrMore WORD)) (pp.OneOrMore "‾") "\\ {" "{{" "1"
                        (t_orn "_CONTENT" CONTENT)
                        ";" (pp.OneOrMore "_") "/ }}" "}1"))

    (defn #^ (of List FGroup)
        find_fgroups [#^ str code]
        (setv _groups (FPTK_GROUP.search_string code))
        (lmap (fm (FGroup :name        %1._NAME
                          :raw_content %1._CONTENT))
              _groups ))

; _____________________________________________________________________________/ }}}1

    ; lmap everywhere is honestly not required, since normally only 1 elem is expected (but hey)
; find import_module ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (setv FPTK_IM (+ LPAR "import"
                     (t_orn "_NAME" WORD)
                     RPAR
                     (pp.Optional (+ (pp.Literal "#_") (t_orn "_DESCR" QSTRING)))))

    (defn #^ (of List FEntity)
        process_IMs [#^ str code]
        (setv _IMs (FPTK_IM.search_string code))
        (lmap (fm (FEntity :kind          FEntityKind.IMPORT_MODULE
                           :kind_str      ""
                           :org_name      ""
                           :name          %1._NAME
                           :parent_module ""
                           :signature     ""
                           :descr         (remove_quotes %1._DESCR)))
              _IMs))

; _____________________________________________________________________________/ }}}1
; find import_from_module ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (setv FPTK_IFM (+ LPAR "import"
                      (t_orn "_PARENT_MODULE" WORD)
                      LBRCKT
                      (t_orn "_NAME" WORD)
                      RBRCKT
                      RPAR
                      (pp.Optional (+ (pp.Literal "#_") (t_orn "_DESCR" QSTRING)))))

    (defn #^ (of List FEntity)
        process_IFMs [#^ str code]
        (setv _IFMs (FPTK_IFM.search_string code))
        (setv pipe
            (rcompose (fm (FEntity :kind          FEntityKind.IMPORT_FROM_MODULE
                                   :kind_str      ""
                                   :org_name      ""
                                   :name          %1._NAME
                                   :parent_module %1._PARENT_MODULE
                                   :signature     ""
                                   :descr         (remove_quotes %1._DESCR)))
                      split_descr))
        (lmap pipe _IFMs))

; _____________________________________________________________________________/ }}}1
; find import_from_module_as ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (setv FPTK_IFMA (+ LPAR "import"
                       (t_orn "_PARENT_MODULE" WORD)
                       LBRCKT
                       (t_orn "_ORG_NAME" WORD)
                       ":as"
                       (t_orn "_NAME" WORD)
                       RBRCKT
                       RPAR
                       (pp.Optional (+ (pp.Literal "#_") (t_orn "_DESCR" QSTRING)))))

    (defn #^ (of List FEntity)
        process_IFMAs [#^ str code]
        (setv _IFMAs (FPTK_IFMA.search_string code))
        (setv pipe
            (rcompose (fm (FEntity :kind          FEntityKind.IMPORT_FROM_MODULE_AS
                                   :kind_str      ""
                                   :org_name      %1._ORG_NAME
                                   :name          %1._NAME
                                   :parent_module %1._PARENT_MODULE
                                   :signature     ""
                                   :descr         (remove_quotes %1._DESCR)))
                      split_descr))
        (lmap pipe _IFMAs))

; _____________________________________________________________________________/ }}}1
; find require_macro ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (setv FPTK_RM  (+ LPAR "require"
                      (t_orn "_PARENT_MODULE" WORD)
                      LBRCKT
                      (t_orn "_NAME" WORD)
                      RBRCKT
                      RPAR
                      (pp.Optional (+ (pp.Literal "#_") (t_orn "_DESCR" QSTRING)))))

    (defn #^ (of List FEntity)
        process_RMs [#^ str code]
        (setv _RMs (FPTK_RM.search_string code))
        (setv pipe
            (rcompose (fm (FEntity :kind          FEntityKind.REQUIRE_MACRO
                                   :kind_str      ""
                                   :org_name      ""
                                   :name          %1._NAME
                                   :parent_module %1._PARENT_MODULE
                                   :signature     ""
                                   :descr         (remove_quotes %1._DESCR)))
                      split_descr))
        (lmap pipe _RMs))


; _____________________________________________________________________________/ }}}1
; find non_import_info ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (setv FPTK_NII (+ LPAR "comment" (t_orn "_DESCR" QSTRING) RPAR))

    (defn #^ (of List FEntity)
        process_NIIs [#^ str code]
        (setv _NIIs (FPTK_NII.search_string code))
        (setv pipe
            (rcompose (fm (FEntity :kind          FEntityKind.NON_IMPORT_INFO
                                   :kind_str      ""
                                   :org_name      ""
                                   :name          ""
                                   :parent_module ""
                                   :signature     ""
                                   :descr         (remove_quotes %1._DESCR)))
                      split_descr))
        (lmap pipe _NIIs))

; _____________________________________________________________________________/ }}}1
; find defined_setv ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (setv FPTK_DS   (+ (pp.Optional (+ (pp.Literal "#_") (t_orn "_DESCR" QSTRING)))
                       LPAR "setv"
                       (t_orn "_NAME" WORD)
                       CONTENT))

    (defn #^ (of List FEntity)
        process_DSs [#^ str code]
        (setv _DSs (FPTK_DS.search_string code))
        (lmap (fm (FEntity :kind          FEntityKind.DEFINED_SETV
                           :kind_str      ""
                           :org_name      ""
                           :name          %1._NAME
                           :parent_module $FPTK_NAME
                           :signature     ""
                           :descr         (remove_quotes %1._DESCR)))
              _DSs))

; _____________________________________________________________________________/ }}}1
; find defined_func ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (setv FPTK_DF   (+ (pp.Optional (+ (pp.Literal "#_") (t_orn "_DESCR" QSTRING)))
                       LPAR "defn"
                       (pp.Optional QEXPR)      ; decorator
                       (pp.Optional ANNOTATION) ; return type
                       (t_orn "_NAME" WORD)     ; name
                       CONTENT
                       RPAR))

    (defn #^ (of List FEntity)
        process_DFs [#^ str code]
        (setv _DFs (FPTK_DF.search_string code))
        (setv pipe
            (rcompose (fm (FEntity :kind          FEntityKind.DEFINED_FUNC
                                   :kind_str      ""
                                   :org_name      ""
                                   :name          %1._NAME
                                   :parent_module $FPTK_NAME
                                   :signature     ""
                                   :descr         (remove_quotes %1._DESCR)))
                      split_descr))
        (lmap pipe _DFs))

; _____________________________________________________________________________/ }}}1

; find all ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (setv FPTK_ALL (| (t_orn "_IM"   FPTK_IM  )
                      (t_orn "_IFM"  FPTK_IFM )
                      (t_orn "_IFMA" FPTK_IFMA)
                      (t_orn "_RM"   FPTK_RM  )
                      (t_orn "_NII"  FPTK_NII )
                      (t_orn "_DF"   FPTK_DF  )
                      (t_orn "_DS"   FPTK_DS  )))

    (defn #^ (of List FEntity)
        find_all
        [#^ FGroup fgroup]
        (setv _ALL (FPTK_ALL.search_string fgroup.raw_content))
        (setv _fentities
            (lfor &elem _ALL
                ; «first» extracts found string (to send raw code further)
                (cond (!= &elem._IM   "") (process_IMs   (first &elem))
                      (!= &elem._IFM  "") (process_IFMs  (first &elem))
                      (!= &elem._IFMA "") (process_IFMAs (first &elem))
                      (!= &elem._RM   "") (process_RMs   (first &elem))
                      (!= &elem._NII  "") (process_NIIs  (first &elem))
                      (!= &elem._DS   "") (process_DSs   (first &elem))
                      (!= &elem._DF   "") (process_DFs   (first &elem)))))
        (flatten _fentities))

; _____________________________________________________________________________/ }}}1

    ; PRINTER: 

; md file header str ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (setv $HEADER
"
---
fptk functions, macroses and imported modules:
1. You are here -> [Functions and modules](https://github.com/rmnavr/fptk/blob/main/docs/functions.md)
2. [Basic macros](https://github.com/rmnavr/fptk/blob/main/docs/macros.md)
3. [Lens related macros](https://github.com/rmnavr/fptk/blob/main/docs/lens.md)
---

# Auto-generated full list of FPTK entities (except macros)

## Legend

List below has format:
```hy
=== Group name 1 ===
TYPE source_lib | func_or_class_name :: signature ; description
TYPE source_lib | func_or_class_name :: signature ; description

=== Group name 2 ===
...
```

Column `TYPE` shows if things are simple imports/reimports: ...
```hy
FULL MODULE  | sys          ; (import sys)
FROM: math   | ln (<-log)   ; (import math [log :as ln])
MACR: hyrule | of           ; (require hyrule [of])
INFO: hy     | cut /macro/  ; shows info on hy/py functions/macro (which are already always in main context); given just for big picture
```

... or fptk-defined entities:
```hy
SETV: fptk   | StrictNumber ; entity defined internally via (setv ...)
DEFN: fptk   | third        ; entity defined internally via (defn ...)
```

## List of fptk entities
")


; _____________________________________________________________________________/ }}}1
; entity to str ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (defn #^ str build_prefix [ #^ FEntity fe ]
        (setv signature (if (= fe.signature "")
                            ""
                            (sconcat " :: " fe.signature " ")))
        (setv descr     (if (= fe.descr "")
                            ""
                            (sconcat " ; " fe.descr) ))
        (return (sconcat signature descr)))

    (defn entity2str [ #^ FEntity fe ]
        (setv pad1 (fm (pad_string %1 15)))
        (setv pad2 (fm (pad_string %1 24)))
        ;
        (case fe.kind
              ;
              FEntityKind.IMPORT_MODULE
              (sconcat "FULL MODULE           | " (pad2 fe.name) (build_prefix fe))
              ;
              FEntityKind.IMPORT_FROM_MODULE
              (sconcat "FROM: " (pad1 fe.parent_module) " | " (pad2 fe.name) (build_prefix fe))
              ;
              FEntityKind.IMPORT_FROM_MODULE_AS
              (sconcat "FROM: " (pad1 fe.parent_module) " | " (pad2 (sconcat fe.name " (<-" fe.org_name ")")) (build_prefix fe))
              ;
              FEntityKind.REQUIRE_MACRO 
              (sconcat "MACR: " (pad1 fe.parent_module) " | " (pad2 fe.name) (build_prefix fe))
              ;
              FEntityKind.NON_IMPORT_INFO 
              (sconcat "INFO: " (pad1 fe.parent_module) " | " (pad2 (sconcat fe.name " /" fe.kind_str "/")) (build_prefix fe))
              ;
              FEntityKind.DEFINED_SETV 
              (sconcat "SETV: " (pad1 fe.parent_module) " | " (pad2 fe.name) (build_prefix fe))
              FEntityKind.DEFINED_FUNC
              (sconcat "DEFN: " (pad1 fe.parent_module) " | " (pad2 fe.name) (build_prefix fe))))

; _____________________________________________________________________________/ }}}1
; group to str ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (defn #^ (of List str)
        group2str_list [ #^ FGroup fgroup ]
        (flatten [ (sconcat "=== " fgroup.name " ===")
                   (lmap (p: entity2str
                             rstrip) ; rstrip removes possible spaces on the right (to not invoke «next line» in *.md)
                         (find_all fgroup)) 
                   "" ]))

; _____________________________________________________________________________/ }}}1
; printers ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (defn gprint
        [ #^ FGroup fgroup ]
        (lprint (group2str_list fgroup)))

    (defn generate_and_write_doc
        [ #^ str source_file
          #^ str output_file
          #^ str [printQ False] #_ "will also print table of functions (without header) into stdout"
        ]
        (setv _code       (read_file source_file))
        (setv _groups     (find_fgroups _code))
        (setv _outp_table (->> _groups (lmap group2str_list)
                                     flatten
                                     (str_join :sep "\n")
                                     ))
        (setv _outp_all   (sconcat $HEADER "\n```hy\n" _outp_table "```"))
        (write_file _outp_all output_file)
        ;
        (print "File" output_file "written!")
        (when printQ (print _outp_table)))

; _____________________________________________________________________________/ }}}1

    (setv $SOURCE "../../fptk/fptk_funcs.hy")
    (setv $OUTPUT "../../docs/functions.md")
    (generate_and_write_doc $SOURCE $OUTPUT :printQ False)

