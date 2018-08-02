;;; bap-mode.el --- Major-mode for BAP's IR
;;
;; Copyright (c) 2017 Henrik Lissner (since based on emacs-mips-mode)
;; Copyright (C) 2018 Thomas Barabosch
;;
;; Author: Thomas Barabosch <http://github/tbarabosch>
;; Maintainer: Thomas Barabosch <thomas.barabosch@fkie.fraunhofer.de>
;; Created: January 18, 2018
;; Modified: August 02, 2018
;; Version: 0.1
;; Keywords: languages
;; Homepage: https://github.com/fkie-cad/bap-mode
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; bap-mode enables you to read and study the intermediate representation (BIR) emitted by the
;; Binary Analysis Platform (BAP).
;;
;;; Code:

(defgroup bap nil
  "Major mode for reading BAP's BIR"
  :prefix "bap-"
  :group 'languages
  :link '(url-link :tag "Github" "https://github.com/fkie-cad/bap-mode"))

(defconst bap-keywords
  '(
    "goto"
    "when"
    "call"
    "return"
    "noreturn"
    "high"
    "low"
    "in"
    "out"
    "bt"
    "u1"
    "u32"
    "u64"
    "u128"
    "extract"
    "with"
    "pad"
    "extend"
    "el"
    "be"
    "phi"
    "ite"
    "unknown"
    "interrupt"))

(defconst bap-defs
  '("program"
    "sub"))

(defconst bap-operators
  '(":="
    "-"
    "+"
    "/"
    "/$"
    "<<"
    ">>"
    "~>>"
    "&"
    "|"
    "^"
    "="
    "*"
    "<>"
    "<"
    "<="
    "<$"
    "<=$"
    "%"
    "<-"
    "~"))

(defconst bap-font-lock-defaults
  `((
     ;; keywords
     (,(regexp-opt bap-keywords 'words) . font-lock-keyword-face)
     ;; labels
     ("[%][a-f0-9]+\\|[a-f0-9]+:\\|@?sub_[0-9a-fA-F]+\\|@[a-z_]+\\|[_a-z][_0-9a-z]+\(" . font-lock-function-name-face)
     ;; numbers
     ("\\_<-?[0-9]+\\>\\|0x[a-fA-F0-9]+" . font-lock-constant-face)
     ;; operators and stuff
     (,(regexp-opt bap-operators) . font-lock-builtin-face)
     ;; registers
     ("[COSZ]F\\|R[ABCD]X\\|R[DS]I\\|R[BS]P\\|R[0-9]+\\|SP\\|LR\\|[NV]F\\|E[ABCDS][IPX]\\|YMM[0-9]\\|mem" . font-lock-type-face)
     ;; variables
     ("div[0-9]+\\|rem[0-9]+\\|tmp[0-9]+\\|[vo][0-9]+\\|[_a-z][_a-z]+[:space:]+::" . font-lock-variable-name-face)
     ;; defs
     (,(regexp-opt bap-defs) . font-lock-preprocessor-face)
     )))

(defvar bap-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c C-b l") 'bap-goto-label)
    (define-key map (kbd "C-c C-b d") 'bap-goto-function-definition)
    (define-key map (kbd "C-c C-b m") 'bap-goto-main)
    (define-key map (kbd "C-c C-b o") 'bap-open-file)
    (define-key map (kbd "C-c C-b p") 'bap-open-file-with-extra-pass)
    map)
"Keymap for ‘bap-mode’.")

(defun bap-goto-label (&optional label)
  "Jumps to a user-specified label, if no LABEL is provided to the function."
  (interactive)
  (let ((label (or label (read-minibuffer "Go to Label: "))))
    (goto-char (point-min))
(re-search-forward (format "%s:" label))))

(defun bap-goto-main ()
  "Jumps to main function."
  (interactive)
  (bap-goto-function-definition "main"))

(defun bap-goto-function-definition (&optional definition)
  "Jumps to a user-specified function definition, if no DEFINITION is provided to the function."
  (interactive)
  (let ((definition (or definition (read-minibuffer "Go to Function Definition: "))))
    (goto-char (point-min))
(re-search-forward (format "sub %s(" definition))))

(defun bap-open-file ()
  "Opens a user-specified file with BAP and emits the IR."
  (interactive)
  (let ((filename (expand-file-name (read-file-name "File to Open: "))))
    (with-output-to-temp-buffer (format "%s.bir" filename)
      (insert (shell-command (format "bap %s -d" filename) (format "%s.bir" filename)))
      (pop-to-buffer (format "%s.bir" filename))
      (bap-mode))))

(defun bap-open-file-with-extra-pass ()
  "Opens a user-specified file with BAP using a user-spcified pass and emits the IR."
  (interactive)
  (let ((filename (expand-file-name (read-file-name "File to Open: ")))
	(passnames (read-minibuffer "Select passes to run BAP with, e.g. callsites or callsites,run: ")))
    (with-output-to-temp-buffer (format "%s.bir" filename)
      (insert (shell-command (format "bap %s --pass=%s -d" filename passnames) (format "%s.bir" filename)))
      (pop-to-buffer (format "%s.bir" filename))
      (bap-mode))))

;;;###autoload
(define-derived-mode bap-mode prog-mode "BAP"
  "Major mode for reading BIR's BIR intermediate representation."
  (setq font-lock-defaults bap-font-lock-defaults)
  (use-local-map bap-map))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.bir\\'" . bap-mode))
(add-to-list 'auto-mode-alist '("\\.bap\\'" . bap-mode))

(provide 'bap-mode)
;;; bap-mode.el ends here
