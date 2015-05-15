;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname chat) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;;;;;;;;;;;;;;Lab 12
;;;Burleigh
;;;Peter
(require 2htdp/image)
(require 2htdp/universe)

; An Editor is (make-editor Lo1S Lo1S) 
; An Lo1S is one of: 
; – empty 
; – (cons 1String Lo1S)

(define-struct editor [pre post])
;Where the pre is the text to the left of the cursor that is reversed
;The post is the code to the right of the cursor


; constants 
(define HEIGHT 20) ; the height of the editor 
(define WIDTH 500) ; its width 
(define FONT-SIZE 16) ; the font size 
(define FONT-COLOR "black") ; the font color 
(define LINE-HEIGHT 18);;height of one line of text
(define MAX-LINES 50);;Max number of lines
(define WINDOW-HEIGHT (* LINE-HEIGHT MAX-LINES));;Height of the window
(define BG (frame (rectangle WIDTH WINDOW-HEIGHT 'solid 'white)))
(define LINE-SPACER 2)


; graphical constants 
(define MT (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1 HEIGHT "solid" "red"))



;;create-editor: string string ---> make-editor 
;;The first string is the text to the left of the cursor
;;and the second string is the text to the right of the cursor.
(check-expect (create-editor "somthing" "")
              (make-editor (list "g" "n" "i" "h" "t" "m" "o" "s") empty))
(check-expect (create-editor "some" "thing about")
              (make-editor (list "e" "m" "o" "s") (list "t" "h" "i" "n" "g" " " "a" "b" "o" "u" "t")))

(define (create-editor s1 s2)
  (make-editor (reverse (explode s1))
               (explode s2)))


; Editor -> Image
; renders an editor as an image of the two texts separated by the cursor 
(check-expect (editor-render (create-editor "some" "thing about"))
              (overlay/align "left" "middle" (beside (text "some" FONT-SIZE FONT-COLOR)
                                                     CURSOR
                                                     (text "thing about" FONT-SIZE FONT-COLOR))
                             MT))



(define (editor-render e)
  (overlay/align
   "left" "middle"
   (beside 
    (text (implode (reverse (editor-pre e))) FONT-SIZE FONT-COLOR)
    CURSOR
    (text (implode (editor-post e)) FONT-SIZE FONT-COLOR))
   MT))


; An Editor-ke is one of:
; "left"
; "right"
; "del"
; an alphabetic 1string
; a numeric 1string
; some ohter keyevent, which should be ignored

; Editor KeyEvent -> Editor
; deals with a key event, given some editor
(check-expect (editor-kh (create-editor "" "") "e")
              (create-editor "e" ""))
(check-expect (editor-kh (create-editor "cd" "fgh") "e")
              (create-editor "cde" "fgh"))
(check-expect (editor-kh (create-editor "asd" "bfs") "1")
              (create-editor "asd1" "bfs"))
(check-expect (editor-kh (create-editor "ds" "ah") "left")
              (create-editor "d" "sah"))
(check-expect (editor-kh (create-editor "asgs" "srjesu") "right")
              (create-editor "asgss" "rjesu"))
(check-expect (editor-kh (create-editor "asfagq" "sh8g") "\b")
              (create-editor "asfag" "sh8g"))
(check-expect (editor-kh (create-editor "acbd" "sd5s55") "\t")
              (create-editor "acbd" "sd5s55"))

(define (editor-kh ed k)
  (cond
    [(key=? k "left")
     (if (empty? (editor-pre ed))
         ed
         (make-editor (rest (editor-pre ed))
                      (cons (first (editor-pre ed))
                            (editor-post ed))))]
    [(key=? k "right")
     (if (empty? (editor-post ed))
         ed
         (make-editor (explode (string-append (first (editor-post ed)) (implode (editor-pre ed))))
                      (rest (editor-post ed))))]
    [(key=? k "\b")
     (if (empty? (editor-pre ed))
         ed
         (make-editor
          (rest (editor-pre ed))
          (editor-post ed)))]
    [(key=? k "\t") ed]
    [(key=? k "\r") ed]
    [(= (string-length k) 1)
     (make-editor (cons k (editor-pre ed)) (editor-post ed))]
    [else ed]))


; editor-main : String -> Editor
; launches the editor given some initial string 
(define (editor-main s)
  (big-bang (create-editor s "")
            (on-key editor-kh)
            (to-draw editor-render)))





;; An OutputLines is a Listof[String], representing
;; all lines of text that have been entered or
;; displayed so far, with the most recent line first.

;; A TerminalWorld is a
;;    (make-terminal OutputLines Editor)
(define-struct terminal [output ed])

(define T1 (make-terminal (list "apple"
                                "orange")
                          (create-editor "pear" "mango")))
(define T2 (make-terminal (list "burleigh"
                                "peter")
                          (create-editor "gavin" "lebron")))
(define T3 (make-terminal empty
                          (create-editor "pear" "mango")))
(define T1000 (make-terminal (list "arnold"
                                   "schwarzenegger")
                             (create-editor "" "")))

(define term1 (make-terminal (list "mad" "dam") 
                             (make-editor 
                              (list "c" "b" "a") (list "d" "e" "f"))))

;; where output contains the past N lines produced,
;; and ed is for the current line being typed.
;; (N represents the maximum number of lines in an
;; OutputLines.)

;; A TerminalKey is either an EditorKey or "\r",
;; representing a keystroke that the terminal program can respond to.


;;;;;;;;;;;IMPORTANT
;;;;;;;;cannot be used in key handler due to taking two arguments

(define PUNCTUATION-LIST
  (list"~"
       "`"
       "!"
       "@"
       "#"
       "$"
       "%"
       "^"
       "&"
       "*"
       "("
       ")"
       "_"
       "+"
       "-"
       "="
       "{"
       "}"
       "|"
       ":"
       "\""
       "<"
       ">"
       "?"
       "-"
       "="
       "["
       "]"
       "\\"
       ";"
       "'"
       ","
       "."
       "/"))

;; punctuation? : String -> Boolean
;; determines if the string is punctuation
(check-expect (punctuation? "'") true)
(check-expect (punctuation? "\"") true)
(check-expect (punctuation? "\\") true)
(check-expect (punctuation? "`") true)

(define (punctuation? s)
  (punctuation-helper s PUNCTUATION-LIST))



;;punctuation-helper: string PUNCTUATION-LIST ---> boolean
;;tells whether the string matches anything on the PUNCTUATION-LIST
(check-expect (punctuation-helper "'" PUNCTUATION-LIST) true)
(check-expect (punctuation-helper "\"" PUNCTUATION-LIST) true)
(check-expect (punctuation-helper "\\" PUNCTUATION-LIST) true)
(check-expect (punctuation-helper "`" PUNCTUATION-LIST) true)

(define (punctuation-helper s PUNCTUATION-LIST)
  (cond
    [(empty? PUNCTUATION-LIST) false]
    [(cons? PUNCTUATION-LIST)
     (if (string=? s (first PUNCTUATION-LIST))
         true
         (punctuation-helper s (rest PUNCTUATION-LIST)))]))


;;delete: TerminalOutput ---> TerminalOutput
;;deletes the last string in the output of the terminal if it is over 54 lines

(define (delete to)
  (cond
    [(empty? to) to]
    [(cons? to)
     (if (> (length to) 54)
         (reverse (rest (reverse to)))
         to)]))

;;terminal-kh: key terminal ----> terminal
;; a key handler that behaves similar to the editor but appends a new
;;line to the OutputLines when the Enter key is pressed.
(check-expect (terminal-kh T1 "\r")
              (make-terminal (list "pearmango" "apple" "orange")
                             (create-editor ""  "")))
(check-expect (terminal-kh T1 "a")
              (make-terminal (list "apple" "orange")
                             (create-editor "peara"  "mango")))
(check-expect (terminal-kh T2 "\r")
              (make-terminal (list "gavinlebron" "burleigh" "peter")
                             (create-editor ""  "")))
(check-expect (terminal-kh T3 "\r")
              (make-terminal (list "pearmango")
                             (create-editor "" "")))
(check-expect (terminal-kh T1000 "\r")
              T1000)

(define (terminal-kh t k)
  (cond
    [(key=? "\r" k)
     (if (and (empty? (editor-pre (terminal-ed t)))
              (empty? (editor-post (terminal-ed t))))
         t
         (make-terminal (cons (string-append (implode (reverse (editor-pre (terminal-ed t))))
                                             (implode (editor-post (terminal-ed t))))
                              (delete (terminal-output t)))
                        (create-editor "" "")))]
    [(or 
      (string-alphabetic? k)
      (string-numeric? k)
      (punctuation? k))
     (make-terminal (terminal-output t) (editor-kh (terminal-ed t) k))]
    [else
     t]))



;;terminal-render : terminal -> image
;;draws the world

(check-expect (terminal-render T1)
              (overlay/align/offset 
               "left" "bottom"
               (above/align
                "left"
                (text "orange" FONT-SIZE FONT-COLOR)
                (text "apple" FONT-SIZE FONT-COLOR)
                (editor-render 
                 (terminal-ed T1)))
               -3 
               0
               BG))

(define (terminal-render t)
  (overlay/align/offset
   "left" "bottom"
   (above/align
    "left"
    (render-output (terminal-output t))
    (editor-render (terminal-ed t)))
   -3 0
   BG))


;;render-output: list ---> image
;;renders the output of a terminal into images
(check-expect (render-output (list "apple""orange" "pear" "banana"))
              (above/align
               "left"
               (text "banana" FONT-SIZE FONT-COLOR)
               (text "pear" FONT-SIZE FONT-COLOR)
               (text "orange" FONT-SIZE FONT-COLOR)
               (text "apple" FONT-SIZE FONT-COLOR)))

(define (render-output ls)
  (cond
    [(empty? ls) (rectangle 0 0 'solid 'green)]
    [(cons? ls)
     (above/align "left"
                  (render-output (rest ls))
                  (text (first ls) FONT-SIZE FONT-COLOR))]))



;;terminal-main: world ---> world
;;calls big bang to run the world

(define starting-terminal 
  (make-terminal (list "")
                 (make-editor empty empty)))



(define (terminal-main t) 
  (big-bang t
            (to-draw terminal-render)
            (on-key terminal-kh)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;                                                                               
;                                                                    ;;         
;                                                                     ;         
;    ;;;    ;;;   ;; ;; ;;; ;;;  ;;;   ;; ;;          ;;;    ;;;   ;;;;    ;;;  
;   ;   ;  ;   ;   ;;    ;   ;  ;   ;   ;;           ;   ;  ;   ; ;   ;   ;   ; 
;    ;;;   ;;;;;   ;      ; ;   ;;;;;   ;            ;      ;   ; ;   ;   ;;;;; 
;       ;  ;       ;      ; ;   ;       ;            ;      ;   ; ;   ;   ;     
;   ;   ;  ;   ;   ;       ;    ;   ;   ;            ;   ;  ;   ; ;   ;   ;   ; 
;   ;;;;    ;;;   ;;;;     ;     ;;;   ;;;;           ;;;    ;;;   ;;;;;   ;;;  
;                                                                               
;                                                                               
;                                                                               

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;A Universe is a Listof[iworld]
;;representing all connected worlds.


;;add-world: Universe iworld -> bundle
;;adds a world that has connected to the server,
;;if its name isn't alread in use
(check-expect (add-world empty iworld1)
              (make-bundle 
               (list iworld1)
               (list (make-mail iworld1 (iworld-name iworld1)))
               empty))

(check-expect (add-world (list iworld1) iworld2)
              (make-bundle 
               (list iworld2 iworld1)
               (list (make-mail iworld1 (list (iworld-name iworld2) 'joined))
                     (make-mail iworld2 (iworld-name iworld2)))
               empty))

(check-expect (add-world (list iworld2 iworld1) iworld1)
              (make-bundle
               (list iworld2 iworld1)
               empty
               (list iworld1)))

(define (add-world u w)
  (cond [(empty? u)
         (make-bundle 
          (list w)
          (list (make-mail w (iworld-name w)))
          empty)]
        [(cons? u)
         (if (kick-world? u w)
             (make-bundle
              u
              empty
              (kick-world u w))
             (make-bundle
              (cons w u)
              (bundle-mail u w)
              empty))]))

;;bundle-mail: Universe iworld -> Listof[Mail]
;;Produces a bundle containing mail to all clients about a world joining
(check-expect (bundle-mail empty iworld1)
              (list (make-mail iworld1 (iworld-name iworld1))))

(check-expect (bundle-mail (list iworld1) iworld2)
              (list (make-mail iworld1 (list (iworld-name iworld2) 'joined))
                    (make-mail iworld2 (iworld-name iworld2))))

(define (bundle-mail u w)
  (cond
    [(empty? u) (list (make-mail w (iworld-name w)))]
    [(cons? u)
     (cons (make-mail (first u) (list (iworld-name w) 'joined))
           (bundle-mail (rest u) w))]))
;;kick-world : Universe iwrold -> Listof[iworld]
;; Produces a list of iworlds getting kicked out
(check-expect (kick-world empty iworld1)
              empty)

(check-expect (kick-world (list iworld1 iworld2) iworld1)
              (list iworld1))

(define (kick-world u w)
  (cond
    [(empty? u) empty]
    [(cons? u)
     (if (string=? (iworld-name (first u)) (iworld-name w))
         (list w)
         (kick-world (rest u) w))]))

;;kick-world? : Universe iworld -> Boolean
;;Checks if an iworld is being kicked
(check-expect (kick-world? empty iworld1)
              false)

(check-expect (kick-world? (list iworld1 iworld2) iworld1)
              true)

(define (kick-world? u w)
  (cond
    [(empty? u) false]
    [(cons? u)
     (or (string=? (iworld-name (first u)) (iworld-name w))
         (kick-world? (rest u) w))]))


;; remove-world : Universe iworld --> bundle
;; notify all users that the given client left; remove & disconnect the client.
(check-expect (remove-world (list iworld1 iworld2 iworld3) iworld3)
              (make-bundle (list iworld1 iworld2)
                           (list (make-mail iworld1
                                            (list (iworld-name iworld3)
                                                  'left))
                                 (make-mail iworld2
                                            (list (iworld-name iworld3)
                                                  'left)))
                           (list iworld3)))
(check-expect (remove-world empty iworld1)
              (make-bundle empty empty (list iworld1)))

(define (remove-world u iw)
  (make-bundle
   (remove iw u)
   (make-disconnect-mail (remove iw u) iw)
   (cons iw empty)))



;; make-disconnect-mail : Listof[iworld] iworld --> Listof[mail]
;; makes a list of mail with message of "(iworld-name iworld) left"
;; being sent to all iworlds in the given list
(check-expect (make-disconnect-mail empty iworld1)
              empty)
(check-expect (make-disconnect-mail (list iworld1 iworld2) iworld3)
              (list (make-mail iworld1
                               (list (iworld-name iworld3) 'left))
                    (make-mail iworld2
                               (list (iworld-name iworld3) 'left))))

(define (make-disconnect-mail ls iw)
  (cond [(empty? ls) empty]
        [(cons? ls)
         (cons (make-mail (first ls) (list (iworld-name iw) 'left))
               (make-disconnect-mail (rest ls) iw))]))

;; handle-client-message : Universe iworld ClientMessage --> bundle
;; processes chat messages from the clients
(check-expect (handle-client-message (list iworld1 iworld2) iworld1 "hello")
              (make-bundle (list iworld1 iworld2)
                           (list (make-mail iworld1
                                            (list 'from (iworld-name iworld1)
                                                  "hello"))
                                 (make-mail iworld2
                                            (list 'from (iworld-name iworld1)
                                                  "hello")))
                           empty))

(define (handle-client-message u iw m)
  (make-bundle u
               (make-chat-mail u iw m)
               empty))

;; make-chat-mail : Listof[iworld] iw m --> Listof[mail]
;; makes a list of mail. each mail is addressed to an iworld in the list
;; with the message " 'form (iworld-name iw) m"

(define (make-chat-mail ls iw m)
  (cond [(empty? ls) empty]
        [(cons? ls)
         (cons (make-mail (first ls) (list 'from (iworld-name iw) m))
               (make-chat-mail (rest ls) iw m))]))

;; main : Number --> Universe
;; runs server code

(define (main n)
  (universe empty
            [on-new add-world]
            [on-msg handle-client-message]
            [on-disconnect remove-world]
            [port n]))



;; A ChatWorld is one of the following:
;;   - a string containing the world's name, if the server has
;;     not yet welcomed us
;;   - a TerminalWorld, if it has

;; fun-for-cw : cw --> a ????
#;
(define (fun-for-cw cw)
  (cond [(string? cw) (fun-for-string)]
        [(terminal? cw)
         (... (fun-for-LOS (terminal-output cw)) ...
              (fun-for-ED (terminal-ed cw)) ...)]))
;; A Package is a (make-package World ClientMessage)

;; A ClientMessage is either
;; a String, or
;; something to ignore.


;; handle-chat-key: ChatWorld KeyEvent -> ChatWorld or Package
;; handles editor keystrokes, and on "Enter," sends the editor to the server.
(check-expect (handle-chat-key "Litel-Gal-Gavin" "y") "Litel-Gal-Gavin")
(check-expect (handle-chat-key "Beeg-Boi-Burleigh" "\r") "Beeg-Boi-Burleigh")
(check-expect (handle-chat-key term1 "\r")
              (make-package (make-terminal (list "mad" "dam")
                                           (make-editor empty empty))
                            "abcdef"))

(define (handle-chat-key cw k)
  (cond [(string? cw) cw]
        [(terminal? cw)
         (if (string=? k "\r")
             (make-package (make-terminal (delete (terminal-output cw))
                                          (make-editor empty empty))
                           (string-append (implode (reverse (editor-pre
                                                             (terminal-ed cw))))
                                          (implode (editor-post
                                                    (terminal-ed cw)))))
             (terminal-kh cw k))]))


;; handle-server-message : ChatWorld Message --> ChatWorld
;; adds the message that is receieved from the server to the ChatWorld
(check-expect (handle-server-message term1 "ur mum")
              (make-terminal (list "ur mum" "mad" "dam")
                             (make-editor (list "c" "b" "a")
                                          (list "d" "e" "f"))))
(check-expect (handle-server-message "Gavin" "I don't know")
              "Gavin")

(define (handle-server-message cw s)
  (cond [(string? cw) (if (string=? cw s)
                          (make-terminal (list s)
                                         (make-editor empty empty))
                          cw)]
        [(terminal? cw)
         (make-terminal (cons (message->string s) (terminal-output cw))
                        (terminal-ed cw))]))
;; message->string : Message --> String
;; converts a Message to a String
(check-expect (message->string "name")
              "name")
(check-expect (message->string (list "name" 'joined))
              "name joined")
(check-expect (message->string (list 'from "name" "chat"))
              "From name: chat")
(check-expect (message->string (list "name" 'left))
              "name left")

(define (message->string m)
  (cond 
    [(and (cons? m)
          (string? (first m))
          (cons? (rest m))
          (empty? (rest (rest m)))
          (symbol? (second m))
          (symbol=? 'joined (second m)))
     (string-append (first m) " " "joined")]
    [(and (cons? m)
          (string? (first m))
          (cons? (rest m))
          (empty? (rest (rest m)))
          (symbol? (second m))
          (symbol=? 'left (second m)))
     (string-append (first m) " " "left")]
    [(and (cons? m)
          (symbol? (first m))
          (cons? (rest (rest m)))
          (empty? (rest (rest (rest m))))
          (string? (second m))
          (string? (third m))
          (symbol=? 'from (first m)))
     (string-append "From" " " (second m) ":" " " (third m))]
    [(string? m) m]))

;; chat-render: cw ------------------> image
;; Takes a chat-world and renders it as an image

(check-expect (chat-render term1)
              (terminal-render term1))

(check-expect (chat-render "GavinIsNotKing")
              (terminal-render 
               (make-terminal empty
                              (make-editor empty empty))))

(define (chat-render cw)
  (cond [(string? cw) 
         (terminal-render 
          (make-terminal empty
                         (make-editor empty empty)))]
        [(terminal? cw)
         (terminal-render cw)]))



;; chat-client: name ip adress port -> cw
;; makes the world
(define (chat-client n ip p)
  (big-bang n
            [name n]
            [register ip]
            [to-draw chat-render]
            [on-key handle-chat-key]
            [on-receive handle-server-message]
            [port p]
            [state true]))






