
    ; Load fptk_macros.hy from sibling folder:
    ; - macros cannot be imported with "require" by same method as functions (probably this is due to how hy imports work)
    ; - for this reason relevant file from "fptk_macros.hy" is generated first

    (import sys)
    (setv _cur_path (get sys.path 0))
    (setv _cur_folder_removed (get (_cur_path.rsplit "\\tests" 1) 0))
    (setv _fptk_path (+ _cur_folder_removed "\\fptk"))

    (setv _macros_code
        (with [file (open (+ _fptk_path "\\fptk_macros.hy") "r" :encoding "utf-8")]
              (file.read)))

    (with [file (open (+ _cur_path  "\\fptk_macros.hy") "w" :encoding "utf-8")]
          (file.write (+ "; this file is auto-copy of ../fptk/fptk_macros.hy,\n"
                         "; copying was done just before tests were executed"
                         "; (I don't know how to otherwise 'requre' macros from sibling folder lol)\n\n"
                         _macros_code)))

    (print "file fptk_macros.hy was copied for testing")
