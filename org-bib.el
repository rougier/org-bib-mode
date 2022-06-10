;;; org-bib-mode.el --- Literate bibliography -*- lexical-binding: t -*-

;; Copyright (C) 2021,2022 Nicolas P. Rougier

;; Author: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; Homepage: https://github.com/rougier/org-bib-mode
;; Keywords: convenience
;; Version: 0.2.0

;; Package-Requires: ((emacs "27.1") (org "9.5"))

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

;; Version 0.2.0
;; - API changes to ease end-usage (hopefully)
;;
;; Version 0.1.1.
;; - Fix a broken dependency (issue #1)

;;; Code:
(require 'org)
(require 'bibtex)
(require 'url)


;; Configuration
;; ----------------------------------------------------------------------------
(defgroup org-bib nil
  "Literate bibliography"
  :group 'convenience
  :prefix "org-bib-")

(defcustom org-bib-library-paths '("~/Documents/Papers")
  "A list of files paths for related PDFs."
  :type '(list directory))

(defcustom org-bib-default-library "~/Documents/Papers/papers.org"
  "Default org library file."
  :type 'file)

(defcustom org-bib-pdftotext "pdftotext"
  "Path to pdftotext executable."
  :type 'file)

(defcustom org-bib-exiftool "exiftool"
  "Path to exiftool executable."
  :type 'file)

(defcustom org-bib-unsorted-header "Unsorted"
  "Header where to add new entry."
  :type 'string)

(defcustom org-bib-note-minimum-size 10
  "Minimal note length to be considered not empty" )

(defcustom org-bib-abstract-minimum-size 10
  "Minimal abstract length to be considered not empty" )

(defcustom org-bib--fold-state t
  "Fold state of the buffer"
  :group nil)


;; Private API


;; This function is critical since most operations require to first
;; identify the key at point. It works by first inspecting the org
;; properties :KEY: and, if absent, by looking for the next bib entry
;; and parse the associated key.
(defun org-bib--key-at-point ()
  "Get entry KEY at point."

  (let ((key (org-entry-get (point) "KEY" t)))
    (when (not key)
      (save-excursion
        (bibtex-next-entry)
        (when (search-forward-regexp "@.+{\\(.+\\),")
          (setq key (substring-no-properties (match-string 1))))))
    (if key key
      (throw 'key-not-found "No key were found on this entry"))))


;; This is used for offering completion when editing an entry
(defun org-bib--collect-keys ()
  "Collect all keys (from org KEY properties)."

  (org-map-entries
   (lambda () (org-element-property :KEY (org-element-at-point)))
   "LEVEL=2"))

;; This is used for offering completion when modifying keywords
(defun org-bib--collect-keywords ()
  "Collect all keywords (from org KEYWORDS properties)."

  (org-map-entries
   (lambda () (org-element-property :KEYWORDS (org-element-at-point)))
   "LEVEL=2"))

;; This is the different file candidates org-bib will search. If you
;; have a specific naming scheme for your PDF, this is the place to
;; add it.
(defun org-bib--file-candidates (&optional key)
  "Return relative filename candidates for entry KEY."

  (let* ((bibitem (org-bib--bibitem key))
         (title (cdr (assoc :title bibitem)))
         (year (cdr (assoc :year bibitem)))
         (doi (cdr (assoc :doi bibitem)))
         (key (cdr (assoc :key bibitem))))
    (list
     (when (and year title) (format "%s - %s.pdf" year title))
     (when doi              (format "%s.pdf" doi))
     (when key              (format "%s.pdf" key)))))


;; Move to the start of an entry (without modifying visibility)
(defun org-bib--goto (&optional key)
  "Move point to entry KEY, at header start."

  (let ((key (or key (org-bib--key-at-point)))
        (found))
    (save-excursion
      (goto-char (point-min))
      (setq found (search-forward-regexp key)))
    (when found
      (goto-char found)
      (org-back-to-heading t)
      (while (> (org-element-property :level (org-element-at-point)) 2)
        (org-up-element)))))

;; Move to the bibtex block of an entry (without modifying visibility)
(defun org-bib--goto-bibtex (&optional key)
  "Move point to entry KEY at bibtex block start."
  
  (let ((key (or key (org-bib--key-at-point))))
    (org-bib--goto key)
    (org-next-block 1)))

;; Move to the abstract section of an entry (without modifying visibility)
(defun org-bib--goto-abstract (&optional key)
  "Move point to entry KEY, at abstract header start."

  (let ((key (or key (org-bib--key-at-point))))
    (org-bib--goto key)
    (org-goto-first-child)))

;; Move to the note section of an entry without modifying visibility
(defun org-bib--goto-note (&optional key)
  "Move point to entry KEY, at note header start."

  (let ((key (or key (org-bib--key-at-point))))
    (org-bib--goto key)
    (org-goto-first-child)
    (org-goto-sibling)))


;; Get bibtex block from entry
(defun org-bib--get-bibtex (&optional key)
  "Get bibtex block from entry KEY."
  
  (save-excursion
    (org-bib--goto-bibtex key)
    (save-restriction
      (widen)
      (org-element-property :value (org-element-at-point)))))

;; Get bibtex abstract from entry
(defun org-bib--get-abstract (&optional key)
  "Get abstract from entry KEY."
  
  (save-excursion  
    (org-bib--goto-abstract key)
    (let* ((element (org-element-at-point))
           (beg (org-element-property :contents-begin element))
           (end (org-element-property :contents-end element)))
      (if (and beg end)
          (save-restriction
            (widen)
            (buffer-substring-no-properties beg end))
        ""))))

;; Get bibtex notes from entry
(defun org-bib--get-note (&optional key)
  "Get notes from entry KEY."

  (save-excursion  
    (org-bib--goto-note key)
    (let* ((element (org-element-at-point))
           (beg (org-element-property :contents-begin element))
           (end (org-element-property :contents-end element)))
      (if (and beg end)
          (save-restriction
            (widen)
            (buffer-substring-no-properties beg end))
        ""))))

(defun org-bib--get-file (&optional key)
  "Return filename associated with entry KEY."
  
  (org-bib--has-file key))

(defun org-bib--get-url (&optional key)
  "Return url associated with entry KEY."
  
  (org-bib--has-url key))

(defun org-bib--get-doi (&optional key)
  "Return doi associated with entry KEY."
  
  (org-bib--has-doi key))

(defun org-bib--has-abstract (&optional key)
  "Return whether entry KEY has an abstract whose length is
greater than 'org-bib-abstract-minimum-size characters."

  (save-excursion 
    (org-bib--goto-abstract key)
    (let* ((element (org-element-at-point))
           (limit org-bib-abstract-minimum-size)
           (beg (org-element-property :contents-begin element))
           (end (org-element-property :contents-end element)))
      (and beg end (> (- end beg) limit)))))

(defun org-bib--has-note (&optional key)
  "Return whether entry KEY has an abstract whose length is
greater than 'org-bib-note-minimum-size characters."

  (save-excursion
    (org-bib--goto-note key)
    (let* ((element (org-element-at-point))
           (limit org-bib-note-minimum-size)
           (beg (org-element-property :contents-begin element))
           (end (org-element-property :contents-end element)))
      (and beg end (> (- end beg) limit)))))

(defun org-bib--has-file (&optional key)
  "Return whether entry KEY has an associated file."

  (save-excursion
    (let* ((bibitem (org-bib--bibitem key))
           (filename (cdr (assoc :file bibitem)))
           (filenames (org-bib--file-candidates key))
           (found nil))
      (dolist (path org-bib-library-paths)
        (let ((path (file-name-as-directory path)))
        
          ;; Automatic path and filename candidates
          (dolist (filename filenames)
            (when (and filename (file-exists-p (concat path filename)))
              (setq found (concat path filename))))

          ;; Relative path (using file field)
          (when (and filename (file-exists-p (concat path filename)))
            (setq found (concat path filename)))

          ;; Absolute path (using file field)
          (when (and filename (file-exists-p filename))
            (setq found filename))))
      found)))

(defun org-bib--has-url (&optional key)
  "Return whether entry KEY has an associated url."

  (let* ((bibitem (org-bib--bibitem key)))
    (cdr (assoc :url bibitem))))

(defun org-bib--has-doi (&optional key)
  "Return whether entry KEY has an associated doi."

  (let* ((bibitem (org-bib--bibitem key)))
    (cdr (assoc :doi bibitem))))

(defun org-bib--bibitem-get (field)
  (cond ((string= field "type")
         (progn (goto-char (point-min))
                (search-forward-regexp "@\\(.+\\){")
                (string-trim (replace-regexp-in-string
                              "[ \n]+" " " (match-string 1)))))
        ((string= field "key")
         (progn (goto-char (point-min))
                (search-forward-regexp "@.+{\\(.+\\),")
                (string-trim (replace-regexp-in-string
                              "[ \n]+" " " (match-string 1)))))
        (t (let ((field (bibtex-text-in-field field)))
             (if field 
               (string-trim (replace-regexp-in-string "[ \n]+" " " field)))))))

(defun org-bib--bibitem (&optional key)
  "Get entry KEY as an association list. Fields are those of the
bibitem and there are also a :key and :type fields."
  
  (let ((bibitem (org-bib--get-bibtex key)))
    (with-temp-buffer
      (insert bibitem)
      `((:type .         ,(org-bib--bibitem-get "type"))
        (:key .          ,(org-bib--bibitem-get "key"))
        (:author .       ,(org-bib--bibitem-get "author"))
        (:editor .       ,(org-bib--bibitem-get "editor"))
        (:title .        ,(org-bib--bibitem-get "title"))
        (:booktitle .    ,(org-bib--bibitem-get "booktitle"))
        (:organization . ,(org-bib--bibitem-get "organization"))
        (:journal .      ,(org-bib--bibitem-get "journal"))
        (:series .       ,(org-bib--bibitem-get "series"))
        (:doi .          ,(org-bib--bibitem-get "doi"))
        (:file .         ,(org-bib--bibitem-get "file"))
        (:url .          ,(org-bib--bibitem-get "url"))
        (:tags .         ,(org-bib--bibitem-get "tags"))
        (:keywords .     ,(org-bib--bibitem-get "keywords"))
        (:volume .       ,(org-bib--bibitem-get "volume"))
        (:number .       ,(org-bib--bibitem-get "number"))
        (:year .         ,(org-bib--bibitem-get "year"))
        (:month .        ,(org-bib--bibitem-get "month"))
        (:publisher .    ,(org-bib--bibitem-get "publisher"))))))

(defvar org-bib-url-icon "")
(defvar org-bib-doi-icon "")
(defvar org-bib-filename-icon "")

(defun org-bib--format-header (&optional key)
  "Formatted org Header org for entry KEY. This should be configurable."
  
  (let* ((bibitem (org-bib--bibitem key))
         (title    (cdr (assoc :title bibitem)))
         (year     (cdr (assoc :year bibitem)))
         (filename (org-bib--get-file key))
         (url      (org-bib--get-url key))
         (doi      (org-bib--get-doi key))
         (prefix   (cond (filename (format "[[%s][%s]]" filename org-bib-filename-icon))
                         (url      (format "[[%s][%s]]" url org-bib-url-icon))
                         (doi      (format "[[%s][%s]]" (concat "http://dx.doi.org/" doi) org-bib-doi-icon))
                         (t        " "))))
    (format "** %s %s (%s)" prefix title year)))

(defun org-bib--update-header (&optional key)
  "Update org header for entry KEY."
  
  (save-excursion
    (org-bib--goto key)
    (let ((header (org-bib--format-header key)))
      (delete-region (line-beginning-position) (line-end-position))
      (insert header))))

(defun org-bib--update-properties (&optional key)
  "Update org properties for entry KEY."
  
  (save-excursion
    (org-bib--goto key)
    (let ((bibitem (org-bib--bibitem key)))
      (dolist (key (mapcar 'car bibitem))
        (let ((property (upcase (substring (format "%s" key) 1)))
              (value (cdr (assoc key bibitem))))
          (when (and value (not (string= property "TAGS"))
                           (not (string= property "FILE")))
            (org-set-property property value)))))))

(defun org-bib--update-tags (&optional key)
  "Update bibtex tags for entry KEY."

  (save-excursion
    (org-bib--goto key)
    (let ((key (or key (org-bib--key-at-point)))
          (org-use-tag-inheritance t)
          (tags  (mapconcat  #'substring-no-properties
                             (org-get-tags-at (point))
                             ", ")))
      (org-bib--goto-bibtex key)
      (bibtex-search-entry key)
      (let ((found (bibtex-search-forward-field "tags" t)))
        (when found
          (goto-char (car (cdr found)))
          (bibtex-kill-field)))
      (bibtex-make-field (list "tags" nil tags) t))))

(defun org-bib--update (&optional key)
  "Update entry KEY (header, properties and tags)"

  (org-bib--update-header key)
  (org-bib--update-properties key)
  (org-bib--update-tags key))

(defun org-bib--doi-from-pdf (pdf)
  "Extract doi from a PDF filename."

  (let ((buffer (current-buffer))
        (doi-regexp
         "\\(10\\.[0-9]\\{4,9\\}/[-+._;()/:A-Z0-9]+[-+_()/:A-Z0-9]\\)"))
    (catch 'found

      ;; First method: parse metadata using exiftool
      (let ((found))
        (shell-command (format "%s %s"
                               org-bib-exiftool
                               (shell-quote-argument pdf)))
        (with-current-buffer "*Shell Command Output*"
          (goto-char (point-min))
          (when (search-forward-regexp doi-regexp nil t)
            (setq found (match-string 1))))
        (when found
          (switch-to-buffer buffer)
          (throw 'found found)))

      ;; Second method: parse first PDF page (using pdftotext) and search DOI
      (let ((found))
        (shell-command (format "%s -f 1 -l 1 -enc UTF-8 %s - | grep -i doi"
                               org-bib-pdftotext
                               (shell-quote-argument pdf)))
        (with-current-buffer "*Shell Command Output*"
          (goto-char (point-min))
          (if (search-forward-regexp doi-regexp nil t)
            (setq found (match-string 1))))
        (when found
          (switch-to-buffer buffer)
          (throw 'found found))))))


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

(defun org-bib--new-from-doi (doi &optional pdf copy)
  "Insert a new entry from a DOI and an optional PDF file."
  
  (let* ((copy (or copy t))
         (bibitem (org-bib--get-crossref-info doi))
         (key (with-temp-buffer (insert bibitem)
                                (org-bib--bibitem-get "key")))
         (year (with-temp-buffer (insert bibitem)
                                 (org-bib--bibitem-get "year")))
         (title (with-temp-buffer (insert bibitem)
                                  (org-bib--bibitem-get "title")))
         (entry (concat
                 (format "** Temporary header (will be updated)\n")
                 (format ":PROPERTIES:\n:KEY: %s\n:END:\n" key)
                 (format "#+begin_src bibtex\n%s#+end_src\n" bibitem)
                 (format "*** Abstract\n\n")
                 (format "*** Notes\n\n")))
         (filename (concat (file-name-as-directory (nth 0 org-bib-library-paths))
                           (format "%s - %s.pdf" year title))))

    (if (> (length (org-map-entries t (format "LEVEL=2&KEY=\"%s\"" key))) 0)
        (message (format "An entry with key %s already exists" key))
      (progn 
        (when (and pdf copy)
          (copy-file pdf filename t t)
          (message "Copying %s to %s" pdf filename))

        (save-excursion
          (goto-char (point-min))
          (when (not (search-forward-regexp
                      (concat "^* " org-bib-unsorted-header ".*$") nil t))
            (message "Unsorted header not found. Creating one.")
            (insert (format "* %s (0)\n" org-bib-unsorted-header))
            (goto-char (point-min)))
          (forward-line 1)
          (save-excursion
            (insert entry))
          (org-bib--update key))))))

(defun org-bib--new-from-pdf (pdf)
  "Insert a new entry from a PDF file."
  
  (let ((doi (org-bib--doi-from-pdf pdf)))
    (if doi
        (org-bib--new-from-doi doi pdf)
      (message (format "Cannot extract DOI from %s" pdf)))))


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

;; Code by Ivan Tadeu Ferreira Antunes Filho
;; https://ivanaf.com/emacs_drag-drop_pdfs_paste_html_custom_templates.html
(defun org-bib--file-insert (uri)
  (if (string= (substring uri 0 7) "file://")
      (org-bib--new-from-pdf (dnd-unescape-uri (substring uri 7)))))
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


;; Public API
;; ----------------------------------------------------------------------------

(defun org-bib--completion (completions)
  ;; See https://emacs.stackexchange.com/questions/8115
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata (display-sort-function . ,#'identity))
      (complete-with-action action completions string pred))))

(defun org-bib--selection-item ()
  "Build a selection item as (key . title)"
  
  (let* ((element (org-element-at-point))
         (org-use-tag-inheritance t)
         (key (org-element-property :KEY element))
         (title (concat "— " (org-element-property :TITLE element))))
    (if key `(,key . ,title))))
                
(defun org-bib--selection ()
  "Select an entry from it s KEY."
  
  (let* ((org-use-property-inheritance nil)
         (collection (org-map-entries #'org-bib--selection-item "KEY={.}"))
         (collection (sort collection
                           (lambda (x y) (string< (substring (car x) -4)
                                                  (substring (car y) -4)))))
         (annotation (lambda (candidate)
                       (concat " " (cdr (assoc (string-trim candidate) collection)))))
         (completion-extra-properties
          `(:annotation-function ,annotation)))
    (completing-read "ENTRY KEY: "
                     (org-bib--completion collection))))


(defun org-bib-goto ()
  "Move point to entry KEY, at header start."

  (interactive)
  (let ((key (org-bib--selection)))
    (org-bib--goto key)
    (org-reveal)
    (org-show-entry)
    (outline-show-children)))

(defun org-bib-edit-abstract ()
  "Edit abstract for entry at point."
  
  (interactive)
  (let ((key (org-bib--key-at-point)))
    (org-bib--goto-abstract key)
    (org-reveal)
    (org-show-entry)
    (forward-line)))

(defun org-bib-edit-note ()
  "Edit note for entry at point."
  
  (interactive)
  (let ((key (org-bib--key-at-point)))
    (org-bib--goto-note key)
    (org-reveal)
    (org-show-entry)
    (forward-line)))

(defun org-bib-edit-keywords (keywords)
  "Set keywords for entry at point."

  (interactive
   (list (completing-read "KEYWORDS: " (org-bib--collect-keywords))))

  (save-excursion
    (let ((key (org-bib--key-at-point)))
      (org-bib--goto key)
      (org-entry-put (point) "KEYWORDS" keywords)
      (org-bib--goto-bibtex key)
      (bibtex-search-entry key)
      (when keywords
        (let ((found (bibtex-search-forward-field "keywords" t)))
          (when found
            (goto-char (car (cdr found)))
            (bibtex-kill-field)))
        (bibtex-make-field (list "keywords" nil keywords) t)))))

(defun org-bib-update ()
  "Update selected entries (default to entry at point)."
  
  (interactive)
  (if (region-active-p)
      (org-map-entries #'org-bib--update "LEVEL=2" 'region)
    (org-bib--update)))


;; Make a new entry from a PDF
(defun org-bib-new-from-pdf (pdf)
  "Make a new entry from a PDF filename"
  
  (interactive
   (list (read-file-name "PDF: ")))
  (org-bib--new-from-pdf pdf))


;; Make a new entry from a DOI
(defun org-bib-new-from-doi (doi)
  "Make a new entry from a DOI"
  
  (interactive
   (list (read-from-minibuffer "DOI: ")))
  (org-bib--new-from-doi doi))


;; Modified global visibility cycling
(defun org-bib-shifttab ()
  "Modified global visibility cycling (2 states only)"
  
  (interactive)
  (if org-bib--fold-state
      (progn (setq org-cycle-separator-lines 0)
             (setq org-bib--fold-state nil)
             (org-shifttab 1))
    (progn (setq org-cycle-separator-lines 2)
           (setq org-bib--fold-state t)
           (org-shifttab 2))))

;; Export bibliography
(defun org-bib-export ()
  "Export bibliography to the associated bib file."
  
  (interactive)
  (let ((bibfile
         (concat (file-name-sans-extension (expand-file-name (buffer-name)))
                 ".bib")))
    (org-babel-tangle nil bibfile "bibtex")
    (message "Bibliography exported to %s" bibfile)))


;; Bitmap to indicate matched entries (following search)
(define-fringe-bitmap 'chevron-right
  [#b011000000
   #b001100000
   #b000110000
   #b000011000
   #b000110000
   #b001100000
   #b011000000] 7 9 '(center nil))

;; Search entries
(defun org-bib-search (match)
  (interactive "MSearch: ")

  (set-window-fringes nil 5 0)
  (remove-overlays nil nil 'org-bib-mark t)
  (unless (string= match "")
    (let ((matcher (cdr (org-make-tags-matcher match t))))
      (save-excursion
        (goto-char (point-min))
        (let ((count 0))
          (while (search-forward-regexp "^\\*\\* .+$" nil t)
            (save-excursion
              (goto-char (match-beginning 0))
              (let ((overlay (make-overlay (+ (point) 1) (line-end-position))))
                (overlay-put overlay 'org-bib-mark t)
                (if (apply matcher '(nil nil 2))
                    (progn
                      (setq count (+ count 1))
                      ;; (overlay-put overlay 'face 'nano-subtle)
                      ;;(overlay-put overlay 'face '(:foreground "black"
                      ;; :inherit 'nano-subtle))
                       (overlay-put overlay 'before-string
                                    (propertize ">" 'display 
                                                (list 'left-fringe
                                                      'chevron-right
                                                      'default)))
                      )
                  (overlay-put overlay 'face 'font-lock-comment-face)))))
          (if (> count 0)
              (message (format "%d match(es)" count))
            (message "No match")))))))


(define-minor-mode org-bib-mode
  "Minor mode for litetate & annotated bibliography" t

  :lighter    "org-bib"
  :keymap     `((,(kbd "C-c C-g") . org-bib-goto)
                (,(kbd "C-c C-a") . org-bib-edit-abstract)
                (,(kbd "C-c C-n") . org-bib-edit-note)
                (,(kbd "C-c C-k") . org-bib-edit-keywords)
                (,(kbd "C-c C-d") . org-bib-new-from-doi)
                (,(kbd "C-c C-p") . org-bib-new-from-pdf)
                (,(kbd "C-c C-e") . org-bib-export)
                (,(kbd "C-c C-s") . org-bib-search)
                (,(kbd "C-c C-u") . org-bib-update))
  (if org-bib-mode (org-bib-mode-on) (org-bib-mode-off)))

(defun org-bib-mode-off ()
  "Uninstall org bib mode"
  
  (remove-hook 'before-save-hook #'org-bib--update-count t))

(defun org-bib-mode-on ()
  "Install org bib mode"

  (org-mode)
  (org-indent-mode)
  (org-hide-block-all)
  ;; (visual-line-mode 0)
  (define-key (current-global-map)
    [remap org-shifttab] #'org-bib-shifttab)
  
  (setq-local org-refile-targets `( (,(buffer-name) :maxlevel . 1)))
  (setq-local org-cycle-separator-lines 2)
  (setq-local org-agenda-files (list (buffer-name)))
  (hl-line-mode)

  (setq org-image-actual-width `( ,(truncate (* (frame-pixel-width) 0.85))))
  (setq org-startup-with-inline-images t)

  ;; (face-remap-add-relative 'org-level-1 :foreground "black")
  (face-remap-set-base 'org-level-2 :inherit 'default)
  (setq-local org-tags-column 1)
  ;; (fringe-mode '(5 . 0))
  (set-window-fringes nil 5 0)
                      
  ;; Set bibtex key format
  (setq bibtex-autokey-titleword-length 0
        bibtex-autokey-name-year-separator ":"
        bibtex-autokey-name-case-convert-function 'capitalize
        bibtex-autokey-year-length 4
        bibtex-autokey-names 1
        bibtex-autokey-titleword-separator ""
        bibtex-autokey-year-title-separator ""
        bibtex-autokey-edit-before-use nil)

  ;; We removed the required-fields to avoir error when retrieving an
  ;; entry from biorxiv
  (setq bibtex-entry-format '(opts-or-alts
                              numerical-fields))
  
  ;; Set a global bibfile base on current buffer
  (let ((bibfile
         (concat (file-name-sans-extension (expand-file-name (buffer-name)))
                 ".bib")))
    (setq org-cite-global-bibliography `(,bibfile)))
  
  ;; Add a local hook on save in order to update counts
  (add-hook 'before-save-hook #'org-bib--update-count 0 t)

  ;; Drag and drop installation
  (add-to-list 'dnd-protocol-alist
               '("^file:" . org-bib--file-dnd-protocol))

  (load-library "bibtex"))

(defun org-bib ()
  "Open default library."

  (interactive)
  (find-file org-bib-default-library)
  (org-bib-mode 1))

(provide 'org-bib)
;;; org-bib-mode.el ends here

