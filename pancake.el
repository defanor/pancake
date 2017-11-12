;;; pancake.el --- Pancake browser interface -*- lexical-binding: t; -*-

;; Copyright (C) 2017 defanor

;; Author: defanor <defanor@uberspace.net>
;; Maintainer: defanor <defanor@uberspace.net>
;; Created: 2017-10-28
;; Keywords: pandoc, web, gopher, browser
;; Homepage: https://defanor.uberspace.net/projects/pancake/
;; Version: 0.1.4

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a rather simple interface to the pancake browser.

;;; Code:

(require 'seq)

(defgroup pancake nil
  "Pancake browser interface."
  :prefix "pancake-"
  :group 'applications)

(defface pancake-color-black-foreground
  '((((type graphic) (class color) (background dark)) :foreground "grey")
    (((type graphic) (class color) (background light)) :foreground "black")
    (t :foreground "black"))
  "Foreground face for black color."
  :group 'pancake)

(defface pancake-color-red-foreground
  '((((type graphic) (class color) (background dark)) :foreground "light coral")
    (((type graphic) (class color) (background light)) :foreground "dark red")
    (t :foreground "red"))
  "Foreground face for red color."
  :group 'pancake)

(defface pancake-color-green-foreground
  '((((type graphic) (class color) (background dark)) :foreground "light green")
    (((type graphic) (class color) (background light)) :foreground "dark green")
    (t :foreground "green"))
  "Foreground face for green color."
  :group 'pancake)

(defface pancake-color-yellow-foreground
  '((((type graphic) (class color) (background dark)) :foreground "light yellow")
    (((type graphic) (class color) (background light)) :foreground "yellow4")
    (t :foreground "yellow"))
  "Foreground face for yellow color."
  :group 'pancake)

(defface pancake-color-blue-foreground
  '((((type graphic) (class color) (background dark)) :foreground "light blue")
    (((type graphic) (class color) (background light)) :foreground "dark blue")
    (t :foreground "blue"))
  "Foreground face for blue color."
  :group 'pancake)

(defface pancake-color-magenta-foreground
  '((((type graphic) (class color) (background dark)) :foreground "pink")
    (((type graphic) (class color) (background light)) :foreground "magenta4")
    (t :foreground "magenta"))
  "Foreground face for magenta color."
  :group 'pancake)

(defface pancake-color-cyan-foreground
  '((((type graphic) (class color) (background dark)) :foreground "light cyan")
    (((type graphic) (class color) (background light)) :foreground "dark cyan")
    (t :foreground "cyan"))
  "Foreground face for cyan color."
  :group 'pancake)

(defface pancake-color-white-foreground
  '((((type graphic) (class color) (background dark)) :foreground "white")
    (((type graphic) (class color) (background light)) :foreground "grey")
    (t :foreground "white"))
  "Foreground face for white color."
  :group 'pancake)

(defface pancake-strike-through
  '((t :strike-through t))
  "A strike-through face."
  :group 'pancake)

(defcustom pancake-command '("pancake" "--embedded")
  "A command that runs pancake, along with its arguments"
  :group 'pancake)

(defvar pancake-buffers '()
  "A list of pancake browser buffers, used to find a buffer to
  use by `pancake-browse-url'.")

(defvar pancake-headings '()
  "A list of headings with their levels.")
(make-variable-buffer-local 'pancake-headings)

(defvar pancake-process-output ""
  "Pancake process's stdout collector.")
(make-variable-buffer-local 'pancake-process-output)

(defvar pancake-process-stderr-output ""
  "Pancake process's stderr collector.")
(make-variable-buffer-local 'pancake-process-stderr-output)

(defvar pancake-process nil
  "Pancake browser process.")
(make-variable-buffer-local 'pancake-process)

(defvar pancake-uri-history '()
  "History of visited URIs, limited by `history-length'.")
(make-variable-buffer-local 'pancake-uri-history)

;;###autoload
(defun pancake ()
  "Run the pancake browser."
  (interactive)
  (let ((p-buf (generate-new-buffer "*pancake*")))
    (display-buffer p-buf)
    (with-current-buffer p-buf
      (pancake-mode)
      (setq pancake-process
            (let ((process-environment
                   (append
                    (list (format "TERM=%s" (if window-system "xterm"
                                              (or (getenv "TERM") "dumb")))
                          (format "INSIDE_EMACS=%s,pancake" emacs-version)
                          (format "COLUMNS=%d" (1- (window-width))))
                    process-environment)))
              (make-process
               :name "pancake"
               :buffer p-buf
               :command pancake-command
               :stderr (make-pipe-process
                        :name "pancake-stderr"
                        :buffer (generate-new-buffer "*pancake-stderr*")
                        :filter 'pancake-process-stderr-filter
                        :sentinel 'pancake-process-stderr-sentinel)
               :filter 'pancake-process-filter
               :sentinel 'pancake-process-sentinel)))
      (push p-buf pancake-buffers)
      (read-only-mode 1))))

;;###autoload
(defun pancake-browse-url (url &optional new-session)
  "Browse an URL with pancake, suitable for setting as
`browse-url-browser-function'."
  (when (or new-session (not (consp pancake-buffers)))
    (pancake))
  (with-current-buffer (car pancake-buffers)
    (process-send-string pancake-process (concat url "\n"))
    (display-buffer (current-buffer))))

(defun pancake-translate-color (name attr)
  "Translate pancake colors into emacs faces."
  (intern (concat "pancake-color-"
                  (downcase (symbol-name name))
                  "-"
                  (symbol-name attr))))

(defun pancake-button-action (button)
  "An action to be invoked on button activation."
  (funcall 'browse-url (button-get button 'uri)))

(defun pancake-print-elem (element)
  "Translate ELEMENT into a string."
  (if (stringp element)
      (insert element)
    (let ((start (point)))
      (pcase element
        (`(fg ,color . ,rest)
         (pancake-print-line rest)
         (add-face-text-property
          start (point) (pancake-translate-color color 'foreground) t))
        (`(style strikethrough . ,rest)
         (pancake-print-line rest)
         (add-face-text-property start (point) 'pancake-strike-through t))
        (`(style ,face . ,rest)
         (pancake-print-line rest)
         (add-face-text-property start (point) face t))
        (`(subscript . ,rest)
         (pancake-print-line rest)
         (add-text-properties start (point) '(display (height 0.75))))
        (`(superscript . ,rest)
         (pancake-print-line rest)
         (add-text-properties start (point) '(display ((raise 0.25)
                                                       (height 0.75)))))
        (`(denotation (math . ,formula) . ,rest)
         (pancake-print-line rest))
        (`(denotation (link . ,uri) . ,rest)
         (pancake-print-line rest)
         (make-text-button start (point)
                           'uri uri
                           'action #'pancake-button-action))
        (`(denotation (heading . ,level) . ,rest)
         (pancake-print-line rest)
         (add-to-list 'pancake-headings (cons (line-number-at-pos) level))
         (add-text-properties start (point)
                              `(display (height ,(1+ (/ 0.5 level))))))
        (_ (format "Unexpected element: %S" element))))))

(defun pancake-print-line (line)
  "Translate LINE (a list of elements) into a string"
  (mapc 'pancake-print-elem line))

(defun pancake-line-p (string)
  "Return t if STRING ends with a newline character."
  (char-equal (elt string (- (length string) 1)) ?\n))

(defun pancake-uri-history-add (uri)
  "Adds URI into `pancake-uri-history', or simply moves it into
the list's `car' if it is already present."
  (setq pancake-uri-history
        (seq-take (cons uri (seq-remove (lambda (e) (string-equal e uri))
                                        pancake-uri-history))
                  history-length)))

(defun pancake-process-filter (proc string)
  "Pancake process filter for stdout."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (setq pancake-process-output (concat pancake-process-output string))
      (when (pancake-line-p pancake-process-output)
        ;; there may be multiple lines, processing separately
        (dolist (raw-line (split-string pancake-process-output "\n"))
          (unless (string-empty-p raw-line)
            (let ((output (read raw-line)))
              (pcase output
                (`(render . ,alist)
                 ;; todo: maybe store identifiers and links for
                 ;; further manipulation
                 (let ((inhibit-read-only t))
                   (delete-region (point-min) (point-max))
                   (setq pancake-headings '())
                   (pancake-uri-history-add (alist-get 'uri alist))
                   (dolist (line (alist-get 'lines alist))
                     (pancake-print-line line)
                     (newline))
                   (goto-char (point-min))))
                (`(goto ,line) (goto-line line))))))
        (setq pancake-process-output "")))))

(defun pancake-process-stderr-filter (proc string)
  "Pancake process filter for stderr."
  (setq pancake-process-stderr-output
        (concat pancake-process-stderr-output string))
  (when (pancake-line-p pancake-process-stderr-output)
    (message "pancake: %s" (string-trim pancake-process-stderr-output))
    (setq pancake-process-stderr-output "")))

(defun pancake-process-sentinel (process event)
  "Pancake process sentinel for stdout."
  (let ((buf (process-buffer process)))
    (when (and (not (process-live-p process))
               (buffer-live-p (process-buffer process)))
      (kill-buffer buf))
    (setq pancake-buffers
          (seq-remove (lambda (x) (eq x buf)) pancake-buffers))))

(defun pancake-process-stderr-sentinel (process event)
  "Pancake process sentinel for stderr."
  (when (and (not (process-live-p process))
             (buffer-live-p (process-buffer process)))
    (kill-buffer (process-buffer process))))

(defun pancake-interrupt ()
  "Send SIGINT to the process."
  (interactive)
  (interrupt-process pancake-process))

(defun pancake-yank ()
  "Insert a string."
  (interactive)
  (funcall (pancake-input (current-kill 0))))

(defun pancake-yank-primary ()
  "Insert a string from the primary selection."
  (interactive)
  (funcall (pancake-input (gui-get-primary-selection))))

(defun pancake-process-send (line)
  "Send LINE to the pancake process."
  (process-send-string pancake-process (concat line "\n")))

(defun pancake-go-backward ()
  "Go backward in history."
  (interactive)
  (pancake-process-send "b"))

(defun pancake-go-forward ()
  "Go forward in history."
  (interactive)
  (pancake-process-send "f"))

(defun pancake-quit ()
  "Quit pancake."
  (interactive)
  (pancake-process-send "q"))

(defun pancake-reload ()
  "Reload the current document."
  (interactive)
  (pancake-process-send "r"))

(defun pancake-display-current-uri ()
  "Display current URI and put it into the kill ring."
  (interactive)
  (when (consp pancake-uri-history)
    (message "%s" (car pancake-uri-history))
    (kill-new (car pancake-uri-history))))

(defun pancake-next-button ()
  "Moves cursor to the next button."
  (interactive)
  (let ((next (next-button (point))))
    (when next
      (goto-char next))))

(defun pancake-previous-button ()
  "Moves cursor to the previous button."
  (interactive)
  (let ((prev (previous-button (point))))
    (when prev
      (goto-char prev))))

(defun pancake-previous-heading ()
  "Moves cursor to the previous heading."
  (interactive)
  (let ((line (seq-find (lambda (x) (< x (line-number-at-pos)))
                        (mapcar 'car pancake-headings))))
    (when line
      (goto-line line))))

(defun pancake-next-heading ()
  "Moves cursor to the next heading."
  (interactive)
  (let ((line (seq-find (lambda (x) (> x (line-number-at-pos)))
                        (reverse (mapcar 'car pancake-headings)))))
    (when line
      (goto-line line))))

(defun pancake-minibuffer-setup ()
  "Removes itself from `minibuffer-setup-hook' and unsets SPC in
that minibuffer, since there's usually no need to complete words,
yet shortcuts should be entered easily."
  (remove-hook 'minibuffer-setup-hook #'pancake-minibuffer-setup)
  (local-unset-key (kbd "SPC")))

(defun pancake-prompt (&optional string)
  "Prompts for input, passes it to `pancake-process'. Similar to
`pancake-input', but runs immediately."
  (interactive)
  (add-hook 'minibuffer-setup-hook #'pancake-minibuffer-setup)
  (pancake-process-send
   (completing-read "pancake> " pancake-uri-history nil nil string)))

(defun pancake-input (string)
  "Pancake input handler: opens minibuffer for input.
Sets the initial contents to STRING, reads the rest, and passes
it to `pancake-process' as input."
  (lambda ()
    (interactive)
    (pancake-prompt string)))

(defvar pancake-mode-map
  (let ((map (make-sparse-keymap))
        (chars (append (list ?? ?. ?/ ?#)
                       (number-sequence ?0 ?9)
                       (number-sequence ?a ?z))))
    (dolist (char chars)
      (let ((str (char-to-string char)))
        (define-key map (kbd str) (pancake-input str))))
    (define-key map (kbd "C-y") 'pancake-yank)
    (define-key map (kbd "<mouse-2>") 'pancake-yank-primary)
    (define-key map (kbd "<mouse-8>") 'pancake-go-backward)
    (define-key map (kbd "<mouse-9>") 'pancake-go-forward)
    (define-key map (kbd "TAB") 'pancake-next-button)
    (define-key map (kbd "<backtab>") 'pancake-previous-button)
    (define-key map (kbd "RET") 'pancake-prompt)
    (define-key map (kbd "C-c C-c") 'pancake-interrupt)
    (define-key map (kbd "C-c C-u") 'pancake-display-current-uri)
    (define-key map (kbd "C-M-e") 'pancake-next-heading)
    (define-key map (kbd "C-M-a") 'pancake-previous-heading)
    (define-key map (kbd "B") 'pancake-go-backward)
    (define-key map (kbd "F") 'pancake-go-forward)
    (define-key map (kbd "Q") 'pancake-quit)
    (define-key map (kbd "R") 'pancake-reload)
    map)
  "Keymap for `pancake-mode'.")

(define-derived-mode pancake-mode nil "Pancake"
  "A basic emacs interface to the pancake browser.")

(provide 'pancake)

;;; pancake.el ends here
