    
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


; CONFIG ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (setv $HYCMD          "hy")
    (setv $FPTK_TESTS_DIR "../../tests")
    (setv $FPTK_TESTS     ["test_macros_import.hy" "tests_main.hy" "test_monad_result.hy"])
    (setv $DOCGEN_DIR     "../doc_generator")
    (setv $DOCGEN         "doc_generator.hy")
    (setv $FPTK_DIR       "../../src/fptk")
    (setv $TIMESTAMP      "_fptk_ver_")

    (setv $VERSION_HEADER "proj_version")
    (setv $SETUP_PY       "../../setup.py")

; _____________________________________________________________________________/ }}}1
; Utils ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    ; 7 = inverse, 4 = underline, 1 = bold
    (defn colorize [n string] (sconcat "[" (str n) "m" string "[0m"))

    ; && chains cmd commands (2nd is run only if 1st was successful)

    (defn run_testN [n]
        (try (run_shell_command f"cd {$FPTK_TESTS_DIR} && {$HYCMD} {(get $FPTK_TESTS n)}")
             (print (colorize 4 (sconcat "Test #" (str (inc n)) " - finished")))
             (except [Exception]
                     (print "ERROR: failed trying to run test" (str (inc n)))
                     (sys.exit 1))))

; _____________________________________________________________________________/ }}}1
    
    ; STEP 1 (tests)

        (print (colorize 7 "[Step 1/3] Running tests:"))
        (run_testN 0) (run_testN 1) (run_testN 2)

    ; STEP 2 (docgen)

        (print "")
        (print (colorize 7 "[Step 2/3] Doc generation for functions:"))

        (try (run_shell_command f"cd {$DOCGEN_DIR} && {$HYCMD} {$DOCGEN}")
             (print (colorize 4 "Docgen - finished"))
             (except [Exception] (print "ERROR: failed trying to run docgen")
                                 (sys.exit 1)))

    ; STEP3 (adding version marker)

        (print "")
        (print (colorize 7 "[Step 3/3] Generating version marker file in fptk dir (and removing previous)"))

        (setv found_prev (lfilter (fm (re_test $TIMESTAMP it))
                                  (os.listdir $FPTK_DIR)))
        (setv found_prev (lmap (partial sconcat $FPTK_DIR "/") found_prev))
        (when (fnot zerolenQ found_prev)
              (print "found prev:" found_prev)
              (lmap os.remove found_prev))

        (setv version (extract_version $VERSION_HEADER (read_file $SETUP_PY)))
        (setv filename (sconcat $FPTK_DIR "/" $TIMESTAMP version))
        (print "writing new:" filename)
        (write_file "" filename)

