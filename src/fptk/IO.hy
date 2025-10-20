
    (export :objects [ file_existsQ fileQ dirQ
                       read_file write_to_file
                     ])

; [GROUP] IO ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (import os.path [exists :as file_existsQ]) #_ "file_existsQ(filename) | also works on folders" ;;
    (import os.path [isfile :as fileQ])        #_ "fileQ(filename) |"
    (import os.path [isdir  :as dirQ])         #_ "dirQ(filename) |"

    #_ "read_file(file_name, encoding='utf-8') -> str | returns whole file content"
    (defn read_file
        [ #^ str file_name
          #^ str [encoding "utf-8"]
        ]
        "returns whole file content"
        (with [file (open file_name "r" :encoding encoding)] (setv outp (file.read)))
        (return outp))

    #_ "write_file(text, file_name, mode='w', encoding='utf-8') | modes: 'w' - (over)write, 'a' - append, 'x' - exclusive creation"
    (defn write_to_file
        [ #^ str text
          #^ str file_name
          #^ str [mode "w"]
          #^ str [encoding "utf-8"]
        ]
        " writes text to file_name;
          modes:
          - 'w' - (over)write
          - 'a' - append
          - 'x' - exclusive creation
          - ...
          - see more at help(open)"
        (with [file (open file_name mode :encoding encoding)] (file.write text)))

; _____________________________________________________________________________/ }}}1

