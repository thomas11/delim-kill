(defun delim-kill (from-char to-char orig-point save)
  "Kill the text between two given characters, preserving balance.

Given FROM and TO, goes backwards from point until it finds FROM
and forward from point until it finds TO, and kills the text
between the two. In addition, the function preserves the balance
between the two characters: For each FROM that is encountered
while looking for TO, one additional TO is required; and vice
versa. For example, in \"{ foo {bar} baz }\", calling the
function with \"{\" as TO and \"}\" as FROM and the cursor being
outside \"{bar}\", the text \" foo {bar} baz \" will be killed,
not \"bar} baz \".

Inspired by Damian Conway's course \"The Productive Programmer\"
and thing-edit by Andy Stewart."
  (interactive "cFrom: \ncTo: \nd\np")
  (let* ((symmetric (eq from-char to-char))
         (from (if symmetric
                   (delim-find-char-backward from-char) 
                 (find-char-balanced-backward to-char from-char)))
         (to (if symmetric
                 (delim-find-char-forward from-char) 
               (find-char-balanced-forward to-char from-char))))
    (if (and from to)
        (delim-kill-it (1+ from) (1- to) save)
      (message "Not found!"))))

(defun delim-find-char (char move-func limit)
  (save-excursion
    (while (progn
             (funcall move-func)
             (not (or (eq (char-after) char)
                      (= (point) limit)))))
  (point)))


(defun find-char-balanced (car counter-char move-func limit)
  (save-excursion
    (let ((skip-count 0))
      (while (progn
               (funcall move-func)
               (when (eq (char-after) char)
                 (setq skip-count (1- skip-count)))
               (when (eq (char-after) counter-char)
                 (setq skip-count (1+ skip-count)))
               (not (or (= skip-count -1)
                        (= (point) limit))))))
    (if (= (point) limit)
        nil
      (point))))


(defun delim-find-char-forward (char)
  (delim-find-char char 'forward-char (point-max)))

(defun delim-find-char-backward (char)
  (delim-find-char char 'backward-char (point-min)))

(defun find-char-balanced-forward (opening closing)
  (find-char-balanced opening closing 'forward-char (point-max)))

(defun find-char-balanced-backward (opening closing)
  (find-char-balanced opening closing 'backward-char (point-min)))


(defun delim-kill-it (from to save)
  (if save
      (progn
        (message "%s saved." (buffer-substring from to))
        (kill-ring-save from to))
    (message "%s copied." (buffer-substring from to))
    (kill-region from to)))


;; Unit tests, using el-expectations by rubikitch,
;; <http://www.emacswiki.org/emacs/EmacsLispExpectations>.
;; ---------------------------------------------------------

(eval-when-compile
  (when (fboundp 'expectations)
    (expectations
      
      (desc "backward")
      (expect " foo {bar} baz "
        (with-temp-buffer 
          (insert "{ foo {bar} baz }")
          (search-backward "baz") 
          (delim-kill (string-to-char "{") (string-to-char "}") (point) nil)
          (car kill-ring-yank-pointer)))

      (desc "forward")
      (expect " foo {bar} baz "
        (with-temp-buffer 
          (insert "{ foo {bar} baz }")
          (search-backward "foo") 
          (delim-kill (string-to-char "{") (string-to-char "}") (point) nil)
          (car kill-ring-yank-pointer)))

                                        ; TODO what's supposed to happen here?
      (desc "on opening char")
      (expect " foo {bar} baz "
        (with-temp-buffer 
          (insert "{ foo {bar} baz }")
          (search-backward "{bar") 
          (delim-kill (string-to-char "{") (string-to-char "}") (point) nil)
          (car kill-ring-yank-pointer)))

                                        ; TODO what's supposed to happen here?
      (desc "on closing char")
      (expect " foo {bar} baz "
        (with-temp-buffer 
          (insert "{ foo {bar} baz }")
          (search-backward "} b") 
          (delim-kill (string-to-char "{") (string-to-char "}") (point) nil)
          (car kill-ring-yank-pointer)))

                                        ; TODO
      (desc "same char")
      (expect "bar"
        (with-temp-buffer 
          (insert "foo bar baz")
          (search-backward "r b") 
          (delim-kill (string-to-char " ") (string-to-char " ") (point) nil)
          (car kill-ring-yank-pointer))))))


;; End unit tests. -----------------------------------------
