;; https://learnxinyminutes.com/docs/elisp/

;; Elisp programs are made of symbolic expressions ("sexps"):
(+ 2 2)

;; this is a comment
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

;; conbine several sexps with `progn';
(progn
  (switch-to-buffer-other-window "*test*")
  (hello "you"))

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

;; refine function
(defun hello (name)
  (insert (format "Hello %s!\n" name)))

(hello "you")

;; create another function which uses `let':
(defun greeting (name)
  (let ((your-name "Derek"))
    (insert (format "Hello %s!\n\nI am %s."
                   name
                   your-name
                   ))))
(greeting "you")
