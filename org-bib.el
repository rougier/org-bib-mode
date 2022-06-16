;;; org-bib.el --- Literate bibliography -*- lexical-binding: t -*-

;; Copyright (C) 2021,2022 Nicolas P. Rougier

;; Author: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; Homepage: https://github.com/rougier/org-bib-mode
;; Keywords: convenience
;; Version: 0.3.0

;; Package-Requires: ((emacs "27.1") ("org-imenu") ("pdf-drop-mode"))

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

;; Version 0.3.0
;; - Complete API change, taking advantage of ol-bibtex, org-imenu and pdf-drop-mode
;; - New features:
;;   - View mode to lock current view to info, abstract, notes, pdf or bibtex
;;   - Export is now made via org-bibtex
;; - API Changes:
;;   - No more bibtex code (generated on demand)
;;   - FILE has been renamed to FILENAME
;;   - TYPE has been renamed to BTYPE

;; Version 0.2.0
;; - API changes to ease end-usage (hopefully)

;; Version 0.1.1.
;; - Fix a broken dependency (issue #1)

;;; Code:

;; Emacs core
(require 'org)
(require 'org-indent)
(require 'org-element)
(require 'ol-bibtex)

;; Needs installation
(require 'org-imenu)
(require 'pdf-drop-mode)

;; ----------------------------------------------------------------------------
(setq bibtex-autokey-titleword-length 0
      bibtex-autokey-name-year-separator ":"
      bibtex-autokey-name-case-convert-function 'capitalize
      bibtex-autokey-year-length 4
      bibtex-autokey-names 1
      bibtex-autokey-titleword-separator ""
      bibtex-autokey-year-title-separator ""
      bibtex-autokey-edit-before-use nil
      imenu-list-position 'left
      imenu-list-size 100
      org-imenu-depth 2
      org-image-actual-width `( ,(truncate (* (frame-pixel-width) 0.85)))
      org-startup-with-inline-images t)

;; (setq header-line-format
;;       '(:eval
;;         (nano-modeline-render nil
;;                               (buffer-name imenu-list--displayed-buffer)
;;                               (format "(view mode: %s, filter: %s)"
;;                                       (if (eq org-bib--view-mode-current 'none)
;;                                           "-"
;;                                         org-bib--view-mode-current)
;;                                       (if (eq org-imenu-filter-string "*")
;;                                           "-"
;;                                         org-imenu-filter-string))
;;                               "")))
;; (face-remap-add-relative 'hl-line :inherit 'nano-strong-i)
   

;; ----------------------------------------------------------------------------
(defgroup org-bib nil
  "Literate bibliography"
  :group 'applications)

(defcustom org-bib-library-file "~/Documents/Papers/papers.org"
  "Default library file (.org)"
  :type 'file
  :group 'org-bib)

(defcustom org-bib-library-path "~/Documents/Papers"
  "Path where to copy newly added files"
  :type 'directory
  :group 'org-bib)

(defcustom org-bib-library-copy-file t
  "Whether to copy new entry files to the library"
  :type 'boolean
  :group 'org-bib)

(defcustom org-bib-library-rename-file t
  "Whether to rename files before copying them to the library"
  :type 'boolean
  :group 'org-bib)

(defcustom org-bib-view-mode-default 'info
  "Initial view mode."
  :type '(radio (const :tag "None"        none)
                (const :tag "Bibtex"      bibtex)
                (const :tag "Abstract"    abstract)
                (const :tag "Notes"       notes)
                (const :tag "PDF"         PDF)
                (const :tag "Information" info))
  :group 'org-bib)

(defcustom org-bib-abstract-section-name "Abstract"
  "Name for the abstract subsection"
  :type 'string
  :group 'org-bib)

(defcustom org-bib-notes-section-name "Notes"
  "Name for the notes subsection"
  :type 'string
  :group 'org-bib)

(defcustom org-bib-unsorted-section-name "Unsorted"
  "Name for the unsorted section where new entries will be filed."
  :type 'string
  :group 'org-bib)

(defvar org-bib--content-buffer nil
  "Buffer holding the current library (org file)")

(defvar org-bib--sidebar-buffer nil
  "Buffer holding the sidebar menu (org imenu)")

(defvar org-bib--refile-history '()
  "Dedicated refile history")

(defvar org-bib--view-mode-current nil
  "Current view mode")

;; This could be user configurable
(defun org-bib-headline-format (entry)
  "Function returning the headline text to use for new entry."
  
  (format "%s (%s)" (cdr (assq :title entry))
                    (cdr (assq :year entry))))

;; This could be user configurable
(defun org-bib-filename-format (entry)
  "Function returning the filename to use for new entry."
  
  (let* ((year (cdr (assq :year entry)))
         (title (cdr (assq :title entry)))
         (title (replace-regexp-in-string "[\\?\\*]" "" title)))
    (format "%s - %s.pdf" year title)))


(defun org-bib-bibtex-from-doi (doi)
  "Retrieve bibtex item using crossref API."
  
  (with-temp-buffer
    (insert (pdf-drop--bibtex-from-doi doi))
    (goto-char (point-min))
    (bibtex-set-dialect 'biblatex t)
    (let ((bibtex-autokey-edit-before-use nil)
          (bibtex-entry-format nil))
      (bibtex-clean-entry t))
    (org-bibtex-read)))

(defun org-bib-bibtex-from-arxiv (arxiv-id)
  "Retrieve bibtex item using arxiv API."
  
  (with-temp-buffer
    (insert (pdf-drop--bibtex-from-arxiv-id arxiv-id))
    (goto-char (point-min))
    (bibtex-set-dialect 'biblatex t)
    (let ((bibtex-autokey-edit-before-use nil)
          (bibtex-entry-format nil))
      (bibtex-clean-entry t))
    (org-bibtex-read)))

(defun org-bib-pdf-process (source-file file-id)
  "Add an entry (file & doi) under the unsorted heading."
  
  (unless (derived-mode-p 'org-mode)
    (error "Buffer needs to be in org-mode"))
  
  (widen)
  (goto-char (point-min))
  (search-forward-regexp (concat "^* " org-bib-unsorted-section-name))
  (cond ((eq 'doi (car file-id))
         (org-bib-bibtex-from-doi (cdr file-id)))
        
        ((eq 'arxiv (car file-id))
         (org-bib-bibtex-from-arxiv (cdr file-id)))

        (t
         (throw (format "Uknown id type (%s)" (car file-id)) nil )))

  (let* ((entry (car org-bibtex-entries))
         (target-file (cond ((not org-bib-library-copy-file)
                             source-file)
                            ((not org-bib-library-rename-file)
                             (concat (file-name-as-directory org-bib-library-path)
                                     (file-name-nondirectory source-file)))
                            (t
                             (concat (file-name-as-directory org-bib-library-path)
                                     (org-bib-filename-format entry))))))

    (setcar org-bibtex-entries
            (append (car org-bibtex-entries)
                    `((:filename . ,target-file)
                      (:status . "UNREAD")
                      (:entered . ,(format-time-string "%FT%T%z")))))

    ;; Copy file to library
    (when org-bib-library-copy-file
      (copy-file source-file target-file t t)      
      (message "Copying %s to %s" source-file target-file)))
  
  (with-temp-buffer
    (org-mode)
    (org-bibtex-write)
    (let ((org-insert-heading-respect-content t)
          (custom-id (org-entry-get (point) "CUSTOM_ID")))
      (org-insert-subheading t)
      (insert org-bib-abstract-section-name)
      (org-entry-put (point) "CUSTOM_ID" (concat custom-id ":abstract"))
      (org-insert-heading t)
      (insert org-bib-notes-section-name)
      (org-entry-put (point) "CUSTOM_ID" (concat custom-id ":notes")))
    (goto-char (point-min))
    (org-cut-subtree))
        
  (org-paste-subtree 2)
  (org-narrow-to-subtree)
  (with-current-buffer "*Ilist*"
    (org-imenu-update)))

(defun org-bib-view-mode (mode)
  "Set view mode, one of 'info, 'abstract, 'notes, 'pdf, 'bibtex or
'none."

  (interactive)
  (cond ((eq mode 'info)     (org-bib-entry-show-info)
                             (setq org-bib--view-mode-current 'info))
        ((eq mode 'abstract) (org-bib-entry-show-abstract)
                             (setq org-bib--view-mode-current 'abstract))
        ((eq mode 'notes)    (org-bib-entry-show-notes)
                             (setq org-bib--view-mode-current 'notes))
        ((eq mode 'pdf)      (org-bib-entry-show-pdf)
                             (setq org-bib--view-mode-current 'pdf))
        ((eq mode 'bibtex)   (org-bib-entry-show-bibtex)
                             (setq org-bib--view-mode-current 'bibtex))
        (t                   (setq org-bib--view-mode-current 'none)))
  (force-mode-line-update))

;; Aliases
(defun org-bib-view-mode-pdf ()      (interactive) (org-bib-view-mode 'pdf))
(defun org-bib-view-mode-info ()     (interactive) (org-bib-view-mode 'info))
(defun org-bib-view-mode-none ()     (interactive) (org-bib-view-mode 'none))
(defun org-bib-view-mode-notes ()    (interactive) (org-bib-view-mode 'notes))
(defun org-bib-view-mode-bibtex ()   (interactive) (org-bib-view-mode 'bibtex))
(defun org-bib-view-mode-abstract () (interactive) (org-bib-view-mode 'abstract))

(defun org-bib-entry-show (&optional mode)
  "Show selected entry according to mode, one of 'info, 'abstract,
'notes, 'pdf, 'bibtex or 'none"
  
  (interactive)
  (save-selected-window
    (save-excursion
      (let ((mode (or mode org-bib--view-mode-current)))
        (cond ((eq mode 'info)     (org-bib-entry-goto-info))
              ((eq mode 'abstract) (org-bib-entry-goto-abstract))
              ((eq mode 'notes)    (org-bib-entry-goto-notes))
              ((eq mode 'pdf)      (org-bib-entry-goto-pdf))
              ((eq mode 'bibtex)   (org-bib-entry-goto-bibtex))
              (t                   nil))))))

;; Aliases
(defun org-bib-entry-show-pdf ()      (interactive) (org-bib-entry-show 'pdf))
(defun org-bib-entry-show-info ()     (interactive) (org-bib-entry-show 'info))
(defun org-bib-entry-show-none ()     (interactive) (org-bib-entry-show 'none))
(defun org-bib-entry-show-notes ()    (interactive) (org-bib-entry-show 'notes))
(defun org-bib-entry-show-bibtex ()   (interactive) (org-bib-entry-show 'bibtex))
(defun org-bib-entry-show-abstract () (interactive) (org-bib-entry-show 'abstract))


(defun org-bib-entry-goto ()
  "Go to selected entry in the org buffer."

  (interactive)
  (let ((entry (imenu-list--find-entry)))
    (when (imenu--subalist-p entry)
      (setq entry (cons (car entry)
                        (get-text-property 0 'marker (car entry)))))
    (imenu-list--goto-entry entry)))

(defun org-bib-entry-goto-info ()
  "Go to selected entry info in the org buffer."
  
  (org-bib-entry-goto))

(defun org-bib-entry-goto-abstract ()
  "Go to selected entry abstract in the org buffer."
  
  (org-bib-entry-goto)
  (org-goto-first-child)
  (org-narrow-to-subtree))

(defun org-bib-entry-goto-notes ()
  "Go to selected entry notes in the org buffer."
  
  (org-bib-entry-goto)
  (org-goto-first-child)
  (org-goto-sibling)
  (org-narrow-to-subtree))

(defun org-bib-entry-goto-bibtex ()
  "Go to selected entry bibtex in a dedicated buffer."
  
  (org-bib-entry-goto)
  (let* ((custom-id (org-entry-get (point) "CUSTOM_ID"))
         (bibtex-buffer (format "*%s:bibtex*" custom-id)))
    (org-narrow-to-subtree)
    (org-bibtex-export-to-kill-ring)
    (with-current-buffer (get-buffer-create bibtex-buffer)
      (erase-buffer)
      (yank)
      (bibtex-mode)
      (bibtex-reformat))
    (switch-to-buffer bibtex-buffer)))

(defun org-bib-entry-goto-pdf ()
  "Go to selected entry pdf in the org buffer."
  
  (org-bib-entry-goto)
  (when (org-entry-get (point) "FILENAME")
    (find-file (org-entry-get (point) "FILENAME"))))

(defun org-bib-entry-mark-read ()
  "Mark selected entry as read."
  
  (interactive)
  (save-selected-window
    (save-excursion
      (org-bib-entry-goto)
      (org-entry-put (point) "STATUS" "READ")))
  (org-imenu-update))

(defun org-bib-entry-mark-read-and-goto-next ()
  "Mark selected entry as read."
  
  (interactive)
  (org-bib-entry-mark-read)
  (org-bib-entry-next))

(defun org-bib-entry-mark-unread ()
  "Mark selected imenu-entry as read."
  
  (interactive)
  (save-selected-window
    (save-excursion
      (org-bib-entry-goto)
      (org-entry-put (point) "STATUS" "UNREAD")))
  (org-imenu-update))

(defun org-bib-entry-mark-unread-and-goto-next ()
  "Mark selected imenu-entry as read."
  
  (interactive)
  (org-bib-entry-mark-unread)
  (org-bib-entry-next))

(defun org-bib-entry-prev ()
  "Move to previous entry."
  
  (interactive)
  (with-current-buffer "*Ilist*"
    (forward-line -1)
    (goto-char (line-beginning-position)))
  (org-bib-entry-show))

(defun org-bib-entry-next ()
  "Move to next entry."
    
  (interactive)
  (with-current-buffer "*Ilist*"
    (forward-line)
    (goto-char (line-beginning-position)))
  (org-bib-entry-show))

(defun org-bib-entry-pdf-next-page ()
  "Go to next page if a PDF is displayed"

  (interactive)
  (save-selected-window
    ;; WARNING: There must be a better way to select the PDF window
    (other-window 1 nil nil)
    (when (derived-mode-p 'pdf-view-mode)
      (pdf-view-next-page-command))))

(defun org-bib-entry-pdf-prev-page ()
  "Go toprevious page if a PDF is displayed"
  
  (interactive)
  (save-selected-window
    ;; WARNING: There must be a better way to select the PDF window
    (other-window 1 nil nil)
    (when (derived-mode-p 'pdf-view-mode)
      (pdf-view-previous-page-command))))

(defun org-bib-from-doi (&optional doi filename)
  "Add a new entry to the library using DOI and FILE (optional)"

  (interactive)
  (let* ((doi  (or doi (read-string (format "Enter DOI: "))))
         (filename (or filename (read-file-name (format "Enter filename: ")))))
    (org-bib-pdf-process filename doi)))


(defun org-bib-from-bibtex (&optional bibitem file)
  "Add a new entry to the library using BIBITEM and FILE (optional)"
  
  (error "Not yet implemented"))

(defun org-bib-entry-move ()
  "Move entry to another section"

  (interactive)
  (save-selected-window
    (org-bib-entry-goto)
    (let ((org-refile-history org-bib--refile-history))
      (with-current-buffer org-bib--content-buffer
        (org-refile)
        (setq org-bib--refile-history org-refile-history)
        (org-imenu-update))
      (org-bib-entry-goto)
      (org-bib-entry-show))))

(defun org-bib-export ()
  "Export library to a bibtex file"
  
  (interactive)
  (save-selected-window
    (save-excursion
      (with-current-buffer org-bib--content-buffer
        (let ((filename (concat (file-name-sans-extension (buffer-file-name)) "." "bib")))
          (org-bibtex filename))))))

(defun org-bib--imenu-filter-format (oldfun element todo tags marker level)
  "Advice function that is used to modify face for unread entries."
  
  (let* ((status (org-element-property ':STATUS element))
         (entry (apply oldfun (list element todo tags marker level))))
    (if (and (string= status "UNREAD") (= level 2))
        (add-face-text-property 0 (length entry)
                                '(:inherit bold) nil entry))
    entry))

(defun org-bib-activate ()
  "Activates org-bib mode."

  ;; We save the current buffer when entering mode to be able to manipulate it
  ;; later since most actions are done through the sidebar buffer.
  (setq org-bib--content-buffer (current-buffer)
        org-bib--sidebar-buffer "*Ilist*")

  (setq org-bibtex-headline-format-function #'org-bib-headline-format)
  (setq pdf-drop-search-hook #'org-bib-pdf-process)
  (setq imenu-list-after-jump-hook #'org-tree-to-indirect-buffer)
  (advice-add 'org-imenu-filter-format :around #'org-bib--imenu-filter-format)
  (setq org-bib--view-mode-current org-bib-view-mode-default)

  (with-current-buffer org-bib--content-buffer
    (setq-local org-refile-targets `( (,(buffer-name) :maxlevel . 1)))
    (setq-local org-agenda-files (list (buffer-name)))
    (pdf-drop-mode)
    (org-imenu))

  (with-current-buffer org-bib--sidebar-buffer
    (org-imenu-update)
    (org-bib-entry-show)

    ;; This prevents the default behavior to override our mouse binding
    (setq-local mouse-1-click-follows-link nil)
    
    (let ((map (current-local-map)))
      (define-key map (kbd "<down-mouse-1>") #'org-bib-entry-show)
      (define-key map (kbd "SPC") #'org-bib-entry-show-info)
      (define-key map (kbd "RET") #'org-bib-entry-goto)
      
      (define-key map (kbd "<down>") #'org-bib-entry-next)
      (define-key map (kbd "<up>")   #'org-bib-entry-prev)

      (define-key map (kbd "<left>")   #'org-bib-entry-pdf-prev-page)
      (define-key map (kbd "<right>")  #'org-bib-entry-pdf-next-page)

      (define-key map (kbd "!") #'org-bib-entry-mark-read-and-goto-next)
      (define-key map (kbd "?") #'org-bib-entry-mark-unread-and-goto-next)

      (define-key map (kbd "m") #'org-bib-entry-move)
      (define-key map (kbd "e") #'org-bib-export)
      
      (define-key map (kbd "i") #'org-bib-entry-show-info)
      (define-key map (kbd "p") #'org-bib-entry-show-pdf)
      (define-key map (kbd "n") #'org-bib-entry-show-notes)
      (define-key map (kbd "b") #'org-bib-entry-show-bibtex)
      (define-key map (kbd "a") #'org-bib-entry-show-abstract)

      (define-key map (kbd "v p")   #'org-bib-view-mode-pdf)
      (define-key map (kbd "v RET") #'org-bib-view-mode-none)
      (define-key map (kbd "v i")   #'org-bib-view-mode-info)
      (define-key map (kbd "v n")   #'org-bib-view-mode-notes)
      (define-key map (kbd "v b")   #'org-bib-view-mode-bibtex)
      (define-key map (kbd "v a")   #'org-bib-view-mode-abstract))))

(defun org-bib-inactivate ()
  "Inactivates org-bib mode."

  (setq org-bibtex-headline-format-function #'(lambda (entry) (cdr (assq :title entry))))
  (advice-remove 'org-imenu-filter-format #'org-bib--imenu-filter-format)
  (setq pdf-drop-search-hook nil)
  (org-imenu-quit)
  (with-current-buffer org-bib--content-buffer
    (pdf-drop-mode -1)
    (widen)))

(define-minor-mode org-bib-mode
  "Minor mode for litetate & annotated bibliography"
  :initial-value t
  :global nil
  :lighter "org-bib"

  (if org-bib-mode
      (org-bib-activate)
    (org-bib-inactivate)))

(provide 'org-bib)
;;; org-bib.el ends here
