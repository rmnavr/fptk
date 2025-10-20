
    (import  _fptk_local *)
    (require _fptk_local *)
    (import os)

; [F] get version in setup.py ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (defn #^ str
        extract_version
        [ #^ str version_header
          #^ str file_content
        ]
        (re_find (sconcat version_header r"\s*=\s*'(.*?)'")
                 file_content
                 re.DOTALL))

; _____________________________________________________________________________/ }}}1
; [F] generate header (for united single file) ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (defn #^ str
        generate_file_header
        [ #^ str version
        ]
        (sconcat "\n" "; This is local version of github.com/rmnavr/fptk lib.\n"
                      "; It's purpose is to have stable fptk inside other projects until fptk reaches stable version.\n"
                      "; This file was generated from local git version: " version
                      "\n\n"))

; _____________________________________________________________________________/ }}}1

; [F] wrap into vim cells ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    ; these are given in such form, because VimCells messes with {({ and such (this is Vim issue)
    (setv $OP3 (smul "{" 3))
    (setv $CL3 (smul "}" 3))

    (defn #^ str
        increase_all_vimcells_depth
        [ #^ str code
        ]
        (sconcat "changes all (((N and )))N to (((N+1 and )))N+1, also making cells headers shorter")
        (re_sub (sconcat r"(‾‾‾‾‾\\ " $OP3 r"|_____/ " $CL3 r")(\d)")
                (fm (sconcat (drop 5 (%1.group 1)) (str (inc (int (%1.group 2))))))
                code))

    (defn #^ str
        wrap_in_new_vimcell
        [ #^ str cell_header
          #^ str code
        ]
        "adds vimcell opener and closer around string"
        (setv opener
              (sconcat "; " cell_header " "
                       (smul "‾" (- 76 (strlen cell_header)))
                       "\\ "
                       (smul "{" 3)
                       "1"))
        (setv closer (sconcat "; _____________________________________________________________________________/ " $CL3 "1"))
        (sconcat opener "\n"
                 (increase_all_vimcells_depth code)
                 "\n"
                 closer
                 "\n"))

; _____________________________________________________________________________/ }}}1
; [F] combine all files ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (defn #^ str
        prepare_one_file
        [ #^ str file_name
          #^ str cell_name
        ]
        (->> (read_file           file_name)
             (wrap_in_new_vimcell cell_name)))

    (defn #^ str
        sanitize_imports_and_exports
        [ #^ str content
        ]
        (->> content
             (re_sub r"\(require\s*fptk\._macros.*?\)" "; ///fptk_local: removed import of fptk._macros///" :flags re.DOTALL)
             (re_sub r"hy\.R\.fptk\." "" )
             (re_sub r"\(export.*?\)" "; ///fptk_local: removed export statement///" :flags re.DOTALL)))

    (defn #^ str
        combine_all_files
        [ #^ (of Tuple str str) filenames_and_cellnames ; #(filename cellname_to_put_content_into)
        ]
        (->> filenames_and_cellnames
             (lmap (fm (prepare_one_file (first it) (second it))) )
             (f> (sconcat #* it))
             (sanitize_imports_and_exports)))

; _____________________________________________________________________________/ }}}1

; CONFIG ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (setv $SOURCES [ #("../../src/fptk/flow.hy"             "[F] flow")
                     #("../../src/fptk/apl.hy"              "[F] apl")
                     #("../../src/fptk/getters.hy"          "[F] getters")
                     #("../../src/fptk/typing.hy"           "[F] typing")
                     #("../../src/fptk/mathnlogic.hy"       "[F] mathnlogic")
                     #("../../src/fptk/strings.hy"          "[F] strings")
                     #("../../src/fptk/IO.hy"               "[F] IO")
                     #("../../src/fptk/lens.hy"             "[F] lens")
                     #("../../src/fptk/benchmark.hy"        "[F] benchmark")
                     #("../../src/fptk/testing.hy"          "[F] testing")
                     #("../../src/fptk/monads/resultM.hy"   "[Monads] resultM")
                     #("../../src/fptk/_macros.hy"          "MACROS")
                   ])

    (setv $SETUP_PY             "../../setup.py")
    (setv $SETUP_VERSION_HEADER "proj_version")

; _____________________________________________________________________________/ }}}1

    (setv _fptk_version (extract_version $SETUP_VERSION_HEADER (read_file $SETUP_PY)))
    (setv _file_content (sconcat (generate_file_header _fptk_version)
                                 (combine_all_files $SOURCES)))

    (setv _target (sconcat "../../_etc/local_versions/" _fptk_version "/_fptk_local.hy"))

    (os.makedirs (os.path.dirname _target) :exist_ok True)
    (write_file _file_content _target)
    (print "file" _target "written!")


    ; "generated_fptk_local/_fptk_local.hy"
