    
    (import  os)
    (import  subprocess)
    (import  _fptk_local *)
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

; Color ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    ; 7 = inverse, 4 = underline, 1 = bold
    (defn colorize [n string] (sconcat "[" (str n) "m" string "[0m"))

; _____________________________________________________________________________/ }}}1
; Run tests ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    ; && chains cmd commands (2nd is run only if 1st was successful)

    (defn run_test [file]
        (try (run_shell_command (sconcat "cd " $FPTK_TESTS_DIR " && " $HYCMD " " file))
             (print (colorize 4 (sconcat "Test " file " - finished")))
             (except [Exception]
                     (print "ERROR: failed trying to run test:" file)
                     (sys.exit 1))))

; _____________________________________________________________________________/ }}}1
; Write version marker ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (defn write_version_marker []
        (setv found_prev (lfilter (fm (re_test $TIMESTAMP it))
                                  (os.listdir $FPTK_DIR)))
        (setv found_prev (lmap (partial sconcat $FPTK_DIR "/") found_prev))
        (when (fnot zerolenQ found_prev)
              (print (colorize 4 "found prev:") found_prev)
              (lmap os.remove found_prev))

        (setv version (extract_version $VERSION_HEADER (read_file $SETUP_PY)))
        (setv filename (sconcat $FPTK_DIR "/" $TIMESTAMP version))
        (print (colorize 4 "writing new:") filename)
        (write_file "" filename))

; _____________________________________________________________________________/ }}}1

; CONFIG ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (setv $HYCMD            "hy")
    (setv $FPTK_DIR         "../../src/fptk")

    (setv $VERSION_HEADER   "proj_version")
    (setv $SETUP_PY         "../../setup.py")
    (setv $TIMESTAMP        "_fptk_ver_")

    (setv $FPTK_TESTS_DIR   "../../tests")
    (setv $FPTK_TESTS       [ "test_macros_import.hy"
                              "tests_main.hy"
                              "test_monad_result.hy"])

    (setv $DOCGEN_DIR       "../01_doc_generator")
    (setv $DOCGEN           "doc_generator.hy")

    (setv $FPTKLOCAL_DIR    "../02_generator_of_fptk_local")
    (setv $FPTKLOCAL        "generator_of_fptk_local.hy")

; _____________________________________________________________________________/ }}}1
    
    (print "")
    (print "Version found in setup.py:" (colorize 4 (extract_version $VERSION_HEADER (read_file $SETUP_PY))))
    (print "")

    ; STEP 1 (tests)

        (print (colorize 7 "[Step 1/4] Running tests:"))
        (lmap run_test $FPTK_TESTS) 

    ; STEP 2 (docgen)

        (print "")
        (print (colorize 7 "[Step 2/4] Doc generation for functions:"))

        (try (run_shell_command f"cd {$DOCGEN_DIR} && {$HYCMD} {$DOCGEN}")
             (print (colorize 4 "Docgen - finished"))
             (except [Exception] (print "ERROR: failed trying to run docgen")
                                 (sys.exit 1)))

    ; STEP 3 (fptk local)

        (print "")
        (print (colorize 7 "[Step 3/4] Generating fptk_local:"))

        (try (run_shell_command f"cd {$FPTKLOCAL_DIR} && {$HYCMD} {$FPTKLOCAL}")
             (print (colorize 4 "Generating _fptk_local - finished"))
             (except [Exception] (print "ERROR: failed trying to generate _fptk_local")
                                 (sys.exit 1)))

    ; STEP4 (adding version marker)

        (print "")
        (print (colorize 7 "[Step 4/4] Generating version marker file in fptk dir (and removing previous)"))
        (write_version_marker)

