;; https://learnxinyminutes.com/docs/elisp/

;; Elisp programs are made of symbolic expressions ("sexps"):
(+ 2 2)

;; From `lisp-interaction-mode' you can evaluate sexps.
;; Put the cursor right after the closing parenthesis then
;; hold down the control and hit the j keys ("C-j" for short).
(+ 3 (+ 1 2))

;; use setq to store value into a variable
(setq my-name "Derek")

;; `insert' will insert "Hello?" where the cursor is.
(insert "Hello!" " world!" my-name)

;; combine sexps into functions:
;; empty parentheses means the function does not accept arguments.
(defun hello () (insert "helllllo, i am" my-name))

;; evaluate function
(hello)

;; accept one argument
(defun hello (name) (insert "Hello " name))

;; call function with string "you" as value for its unique argument
(hello "you")

;; switch to a new buffer named *test* in another window
(switch-to-buffer-other-window "*test*")

;; combine several sexps with `progn';
(progn
  (switch-to-buffer-other-window "*test*")
  (hello "you")
  (hello "are")
  (hello "cool"))

;; erase buffer
(progn
  (switch-to-buffer-other-window "*test*")
  (erase-buffer)
  (hello "derp"))

;; go back to other window:
(progn
  (switch-to-buffer-other-window "*test*")
  (erase-buffer)
  (hello "hello")
  (other-window 1))

;; bind value to local variable with `let'
;; no need to use `progn', since `let' also combines several sexps
(let ((local-name "it's a local varrr"))
  (switch-to-buffer-other-window "*test*")
  (erase-buffer)
  (hello local-name)
  (other-window 1))

;; format string
(format "Hello %s!\n" "visitor")

;; %s is a place-holder for a string, replaced by "visitor".
;; \n is the newline character.

;; refine function
(defun hello (name)
  (insert (format "Hello %s!\n" name)))

(hello "derek yu")

;; create another function which uses `let':
(defun greeting (name)
  (let ((your-name "Derek"))
    (insert (format "Hello %s!\n\nI am %s."
                   name
                   your-name
                   ))))

;; and evaluate it
(greeting "you")

;; some fns are interactive
(read-from-minibuffer "enter your name bb: ")

;; let's make our greeting fn prompt for you name
(defun greeting (from-name)
  (let ((your-name (read-from-minibuffer "enter your name bb: ")))
  (insert (format "hello!\n\nI am %s and you are %s."
                  from-name
                  your-name
                  ))))

(greeting "deeky")

;; complete this by displaying the results on the other window
(defun greeting (from-name)
  (let ((your-name (read-from-minibuffer "enter your name bb: ")))
    (switch-to-buffer-other-window "*test*")
    (erase-buffer)
    (insert (format "hello!\n\nI am %s and you are %s."
                    from-name
                    your-name
                    ))
    (other-window 1)))

(greeting "dory")
