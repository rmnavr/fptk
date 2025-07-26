    
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
                 "\n" closer))

; _____________________________________________________________________________/ }}}1
; [F] relink R imports ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    ; fptk_macros.hy has calls like:
    ; - hy.R.fptk_macros.lns
    ; - hy.R.fptk_macros.fm
    ; 
    ; For them to work correctly in _fptk_local, they need to be updated

    (defn #^ str
        relink_R_imports
        [ #^ str text
        ]
        (re_sub r"(hy\.R\.fptk_macros\.)(lns|fm)" 
                (fm (%1.group 2))
                text))

; _____________________________________________________________________________/ }}}1
; [F] get version in setup.py ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (defn #^ str
        extract_version_from_setup_py 
        [ #^ str setup_py_content
        ]
        (re_find r"setup\(.*version\s*=\s*'(.*?)'"
                 setup_py_content
                 re.DOTALL))

; _____________________________________________________________________________/ }}}1

    (dt_print "[Step 1/4] Tests for functions:")

        (run_shell_command "cd ../tests && hy _util_to_copy_macros.hy")
        (run_shell_command "cd ../tests && hy test_fptk.hy")
        ; && chains cmd commands (2nd is run only if 1st was successful)

    (dt_print "[Step 2/4] Doc generation for functions:")

        (run_shell_command "cd doc_generator && hy doc_generator.hy")

    (dt_print "[Step 3/4] Generating _fptk_local.hy:")

        (setv fptk_funcs_code     (->> (read_file "../fptk/fptk_funcs.hy") (wrap_in_new_vimcell "functions and modules")))
        (setv fptk_macros_code    (->> (read_file "../fptk/fptk_macros.hy") (wrap_in_new_vimcell "macros") relink_R_imports))
        (setv version_in_setup_py (extract_version_from_setup_py (read_file "../setup.py")))

        (write_file (sconcat "\n" "; This is local version of github.com/rmnavr/fptk lib.\n"
                             "; It's purpose is to have stable fptk inside other projects until fptk reaches stable version.\n"
                             "; This file was generated from local git version: " version_in_setup_py 
                             "\n\n" fptk_funcs_code "\n" fptk_macros_code "\n\n")
                    "generated_fptk_local/_fptk_local.hy")

        (print "File generated_fptk_local/_fptk_local.hy written!")

    (print "\n")
    (dt_print "[Step 4/4] Check versions:")

        (print (sconcat "Version in setup.py: " version_in_setup_py "")) 

