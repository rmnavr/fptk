    
    (import os)
    (import subprocess)
    (import _fptk_local *)
    (require _fptk_local *)

; [F] run shell command ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (defn run_shell_command
        [ #^ str  command
          #^ bool [printQ True]
        ]
        (setv result
              (subprocess.run command
                              :shell          True
                              :check          True
                              :text           True
                              :capture_output True))
        (when printQ
            (print result.stdout)
            (print result.stderr)))

; _____________________________________________________________________________/ }}}1
; [F] vim cells ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    ; these are given in such form, because VimCells messes with {({ and such (this is Vim issue)
    (setv $OP3 (smul "{" 3))
    (setv $CL3 (smul "}" 3))

    (defn #^ str
        increase_all_vimcells_depth
        [ #^ str code
        ]
        "changes all (((N and )))N to (((N+1 and )))N+1, also making cells headers shorter"
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
                 "\n" closer))

; _____________________________________________________________________________/ }}}1
; [F] relink R imports ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    ; fptk_macros.hy has calls like:
    ; - hy.R.fptk.lns
    ; - hy.R.fptk.fm
    ; 
    ; For them to work correctly in _fptk_local, it should be relinked 

    (defn #^ str
        relink_R_imports
        [ #^ str text
        ]
        (re_sub r"(hy\.R\.fptk\.)(lns|fm)" 
                (fm (%1.group 2))
                text))

; _____________________________________________________________________________/ }}}1
; [F] get version in setup.py ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (defn #^ str
        extract_version_from_setup_py 
        [ #^ str version_header
          #^ str setup_py_content
        ]
        (re_find (sconcat version_header r"\s*=\s*'(.*?)'")
                 setup_py_content
                 re.DOTALL))

; _____________________________________________________________________________/ }}}1

    (print "\n" "=== FPTK PCKG HELPER =======================================" "\n")

    (setv $HYCMD          "hy")
    (setv $FPTK_TESTS_DIR "../../tests")
    (setv $FPTK_TESTS     "test_fptk.hy")
    (setv $DOCGEN_DIR     "../doc_generator")
    (setv $DOCGEN         "doc_generator.hy")
    (setv $FPTK_FUNCS     "../../src/fptk/fptk_funcs.hy")
    (setv $FPTK_MACROS    "../../src/fptk/fptk_macros.hy")
    (setv $FPTK_LOCAL     "../_generated_fptk_local/_fptk_local.hy")

    (setv $VERSION_HEADER "proj_version")
    (setv $SETUP_PY       "../../setup.py")

    (dt_print "[Step 1/4] Performing tests:")

        (run_shell_command f"cd {$FPTK_TESTS_DIR} && {$HYCMD} {$FPTK_TESTS}")
        ; && chains cmd commands (2nd is run only if 1st was successful)

    (dt_print "[Step 2/4] Doc generation for functions:")

        (run_shell_command f"cd {$DOCGEN_DIR} && {$HYCMD} {$DOCGEN}")

    (dt_print "[Step 3/4] Generating _fptk_local.hy:")

        (setv fptk_funcs_code     (->> (read_file $FPTK_FUNCS)  (wrap_in_new_vimcell "functions and modules")))
        (setv fptk_macros_code    (->> (read_file $FPTK_MACROS) (wrap_in_new_vimcell "macros") relink_R_imports))
        (setv version_in_setup_py (extract_version_from_setup_py $VERSION_HEADER (read_file $SETUP_PY)))

        (write_file (sconcat "\n" "; This is local version of github.com/rmnavr/fptk lib.\n"
                             "; It's purpose is to have stable fptk inside other projects until fptk reaches stable version.\n"
                             "; Due to how hy macros imports work, when you import macros that depend from\n"
                             "; lns and fm (which are l>, f> and others) you need to also import lns and fm.\n"
                             "; fptk lib itself doesn't have this issue.\n\n"
                            f"; This file was generated from local git version: {version_in_setup_py}"
                            f"\n\n{fptk_funcs_code}\n{fptk_macros_code}\n\n")
                    $FPTK_LOCAL)

        (print f"File {$FPTK_LOCAL} written!")

    (print "\n")
    (dt_print "[Step 4/4] Check versions:")

        (print f"Version in setup.py: {version_in_setup_py}") 

    (print "\n" "=== ALL DONE ===============================================" "\n")

