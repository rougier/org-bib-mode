;;; org-bib-mode.el --- Literate bibliography -*- lexical-binding: t -*-

;; Copyright (C) 2021 Nicolas P. Rougier

;; Author: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; Homepage: https://github.com/rougier/org-bib-mode
;; Keywords: convenience
;; Version: 0.1.0

;; Package-Requires: ((emacs "27.1") (org-mode "9.5") (svg-tag-mode "0.3"))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Documentation

;;; News

;;; Code:
(require 'org)
(require 'bibtex)
(require 'svg-tag-mode)

(defgroup org-bib nil
  "Literate bibliography"
  :group 'convenience
  :prefix "org-bib-")

(defcustom org-bib-pdf-directory "~/Papers"
  "Directory where to store pdfs")

(defcustom org-bib-copy-pdf t
  "Whether to copy PDF in org-bib-pdf-directory")

(define-minor-mode org-bib-mode
  "Minor mode for litetate & annotated bibliography."
  :group 'org-bib
  (if org-bib-mode
      (org-bib-mode-on)
    (org-bib-mode-off)))

(define-globalized-minor-mode
   global-org-bib-mode org-bib-mode org-bib-mode-on)

;; Code from https://emacs.stackexchange.com/questions/10245
;; by https://emacs.stackexchange.com/users/2241/olaf-b
(defun org-bib--count-children (&optional level pos match scope)
  "Return number of subentries for entry at POS.
MATCH and SCOPE are the same as for `org-map-entries', but
SCOPE defaults to 'tree.
By default, all subentries are counted; restrict with LEVEL."
  (save-excursion
    (goto-char (or pos (point)))
    ;; If we are in the middle ot an entry, use the current heading.
    (org-back-to-heading t)
    (let* ((maxlevel (when (and level (org-current-level))
                      (+ level (org-current-level))))
          (count (1- (length
                      (delq nil
                            (org-map-entries
                             (lambda ()
                               (or (not maxlevel)
                                   (<= (org-current-level) maxlevel)))
                             match (or scope 'tree)))))))
      count)))

(defun org-bib--update-count ()
  "Update entries count in level 1 headers if they have a (xxx) field."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp "^* .+?\\(\([0-9]+\)\\)" nil t)
      (let ((beg (match-beginning 1))
            (end (match-end 1))
            (count (org-bib--count-children 1)))
        (delete-region beg end)
        (goto-char beg)
        (insert (format "(%d)" count))))))

(defun org-bib--parse-bibtex-key ()
  "Get KEY of current BibTeX entry."
  (save-excursion
    (goto-char (point-min))
    (search-forward-regexp "@.+{\\(.+\\),")
    (string-trim
     (substring-no-properties (match-string 1)))))

(defun org-bib--parse-bibtex-title ()
  "Get content of field TITLE of current BibTeX entry."
  (string-trim (replace-regexp-in-string
                "[ \n]+" " " (bibtex-text-in-field "title"))))

(defun org-bib--parse-bibtex-year ()
  "Get content of field YEAR of current BibTeX entry."
  (string-trim (replace-regexp-in-string
               "[ \n]+" " " (bibtex-text-in-field "year"))))

(defun org-bib--parse-bibtex-doi ()
  "Get content of field DOI of current BibTeX entry."
  (string-trim (bibtex-text-in-field "doi")))

(defun org-bib--get-crossref-info (doi)
  "Retrieve bibtex item using crossref information and DOI."
  
  (let ((url-mime-accept-string "text/bibliography;style=bibtex"))
    (with-current-buffer
      (url-retrieve-synchronously 
         (format "http://dx.doi.org/%s" 
       	         (replace-regexp-in-string "http://dx.doi.org/" "" doi)))
            (setq bibtex-entry 
     	    (buffer-substring 
          	 (string-match "@" (buffer-string))
             (point)))
            (kill-buffer (current-buffer))))
  (with-temp-buffer
    (insert (decode-coding-string bibtex-entry 'utf-8))
    (goto-char (point-min))
    (bibtex-clean-entry (bibtex-generate-autokey))
    (bibtex-fill-entry)
    (buffer-substring (point-min) (point-max))))

;; Code by Ivan Tadeu Ferreira Antunes Filho
;; https://ivanaf.com/emacs_drag-drop_pdfs_paste_html_custom_templates.html
(defun org-bib--file-insert (uri)
  (if (string= (substring uri 0 7) "file://")
      (org-bib-pdf (dnd-unescape-uri (substring uri 7)))))

(defun org-bib--file-dnd-fallback (uri action)
  (let ((dnd-protocol-alist
         (rassq-delete-all 'org-bib--file-dnd-protocol
          (copy-alist dnd-protocol-alist))))
    (dnd-handle-one-url nil action uri)))

(defun org-bib--file-dnd-protocol (uri action)
  (cond ((eq major-mode 'org-mode)
         (condition-case nil
             (org-bib--file-insert uri)
;;           (error
;;            (org-bib--file-dnd-fallback uri action))
           ))
        (t
         (org-bib--file-dnd-fallback uri action))))
;; --- Code by Ivan Tadeu Ferreira Antunes Filho

(defun org-bib--string-fit (str width)
  (let ((width (if (< width 0)
                   (+ (window-width) width)
                 width))
        (ellipsis "…"))
    (if (> (length str) width)
        (format "%s%s" (substring str 0 (- width (length ellipsis))) ellipsis)
      (format (format "%%-%ds"width) str))))

(defun org-bib-pdf (pdf)
  "Make a new entry from a PDF"
  (interactive "MPDF: ")

  ;; Trying to get a DOI from the PDF converted to text (first page only)
  (shell-command (format "pdftotext -f 1 -l 1 %s - | grep -i doi"
                         (shell-quote-argument pdf)))
  (let ((doi ""))
    (with-current-buffer "*Shell Command Output*"
      (goto-char (point-min))
      (search-forward-regexp "\\(10\\.[0-9]\\{4,9\\}/[-+._;()/:A-Z0-9]+\\)")
      (setq doi (match-string 1))
      ;; Some editors add a textual dot at the end of the DOI.
      ;; This confuses the regular expression match and we fix it here.
      (if (string= (substring doi -1) ".")
          (setq doi (substring doi 0 -1))))
    (org-bib-doi doi pdf)))

(defun org-bib-doi (doi &optional pdf)
  "Make a new entry from a DOI"
  (interactive "MDOI: ")

  (let ((bibitem (org-bib--get-crossref-info doi)))
    ;; (message "org-bib-doi/ bibitem: %s" bibitem)
    (with-temp-buffer
      (insert bibitem)
      (let* ((bibkey (org-bib--parse-bibtex-key))
             (year (org-bib--parse-bibtex-year))
             (title (org-bib--parse-bibtex-title))
             (filename (concat (file-name-as-directory org-bib-pdf-directory)
                               (format "%s - %s.pdf" year title)))
             (title (org-bib--string-fit title (- 67 (length bibkey))))
             (date (format-time-string "%% Entry added on %Y-%m-%d at %H:%M\n"))
             (cite (format "[cite:@%s]" bibkey))
             (doi  (if pdf
                       (format "[[%s][|PDF|]]" filename)
                     (format "[[doi:%s][|DOI|]]" (org-bib--parse-bibtex-doi)))))
        (if (and pdf org-bib-copy-pdf)
            (copy-file pdf filename t t)
            (message "Copying %s to %s" pdf filename))
        (setq bibtex-entry
              (concat
               (format "** %s %s %s\n" title cite doi)
               (format "#+begin_src bibtex\n%s%s#+end_src\n\n" date bibitem)
               (format "*** Abstract \n\n")
               (format "*** Notes\n\n/To be written/\n\n"))))))

  (goto-char (point-min))
  (re-search-forward "^* Unsorted.*$")
  (forward-line 2)
  (save-excursion
    (insert bibtex-entry))
  (org-cycle)
  (forward-char 1))

(defun org-bib-export ()
  "Export bibliography to the associated bib file."
  (interactive)
  (let ((bibfile
         (concat (file-name-sans-extension (expand-file-name (buffer-name)))
                 ".bib")))
    (org-babel-tangle nil bibfile "bibtex")
    (message "Bibliography exported to %s" bibfile)))

(defun org-bib-mode-off ()
  "Uninstall org bib mode"
  (remove-hook 'before-save-hook #'org-bib--update-count t)
  (svg-tag-mode -1))

(defun org-bib-mode-on ()
  "Install org bib mode"

  (setq svg-tag-tags
        `(("\\(:no\\)export:" .
           ((lambda (tag) (svg-tag-make "NO"
                                        :face 'org-meta-line
                                        :inverse t
                                        :crop-right t))))
        (":no\\(export:\\)" .
         ((lambda (tag) (svg-tag-make "EXPORT"
                                      :face 'org-meta-line
                                      :crop-left t))))
        ("\\(\\[PDF\\]\\)" .
         ((lambda (tag) (svg-tag-make "PDF"
                                      :face 'org-meta-line
                                      :inverse nil))
          (lambda () (interactive) (call-interactively 'org-bib-pdf))
          "Insert a new entry from a PDF file"))

        ("\\(\\[DOI\\]\\)" .
         ((lambda (tag) (svg-tag-make "DOI"
                                      :face 'org-meta-line
                                      :inverse nil))
          (lambda () (interactive) (call-interactively 'org-bib-doi))
          "Insert a new entry from a DOI"))

        ("\\(|PDF|\\)" .
         ((lambda (tag) (svg-tag-make tag
                                      :beg 1 :end -1
                                      :face 'nano-salient))))
        ("\\(|DOI|\\)" .
         ((lambda (tag) (svg-tag-make tag
                                      :beg 1 :end -1
                                      :face 'nano-popout))))
        ("\\(|WEB|\\)" .
         ((lambda (tag) (svg-tag-make tag
                                      :beg 1 :end -1
                                      :face 'nano-popout))))

        ("\\(\\[cite:@[A-Za-z]+:\\)" .
         ((lambda (tag) (svg-tag-make tag
                                      :face 'nano-salient
                                      :inverse t
                                      :beg 7 :end -1
                                      :crop-right t))))
        ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" .
         ((lambda (tag) (svg-tag-make tag
                                      :face 'nano-salient
                                      :end -1
                                      :crop-left t))))))


  (org-mode)
  (org-indent-mode)
  (org-hide-block-all)
  (svg-tag-mode t)
  (setq-local org-refile-targets `( (,(buffer-name) :maxlevel . 1)))
  (setq-local org-cycle-separator-lines 2)
  (hl-line-mode)
  
  (face-remap-set-base 'org-level-2 :inherit 'default)
  (setq-local org-tags-column 1)

  ;; Set bibtex key format
  (setq bibtex-autokey-titleword-length 0
        bibtex-autokey-name-year-separator ":"
        bibtex-autokey-name-case-convert-function 'capitalize
        bibtex-autokey-year-length 4
        bibtex-autokey-names 1
        bibtex-autokey-titleword-separator ""
        bibtex-autokey-year-title-separator ""
        bibtex-autokey-edit-before-use nil)

  ;; Set a global bibfile base on current buffer
  (let ((bibfile
         (concat (file-name-sans-extension (expand-file-name (buffer-name)))
                 ".bib")))
    (setq org-cite-global-bibliography `(,bibfile)))
  
  ;; Add a local hook on save in order to update counts
  (add-hook 'before-save-hook #'org-bib--update-count 0 t)

  ;; Drag and drop
  (add-to-list 'dnd-protocol-alist
               '("^file:" . org-bib--file-dnd-protocol))

  (load-library "bibtex"))

(provide 'org-bib-mode)
;;; org-bib-mode.el ends here

