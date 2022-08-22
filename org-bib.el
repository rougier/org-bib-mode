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

;; Version 0.4.0
;; - Use of citeproc to render author names in preview
;; - New key bindings to "view and go to"
;;
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
(require 'citeproc)
(require 'oc-csl)
(require 'ol-bibtex)
(require 'xwidget)
(require 'bibtex)


;; Needs installation
(require 'org-imenu)
(require 'pdf-drop-mode)


(defgroup org-bib nil
  "Literate bibliography"
  :group 'applications)


(defcustom org-bib-library-copy-file t
  "Whether to copy new entry files to the library"
  :type 'boolean
  :group 'org-bib)


(defcustom org-bib-library-rename-file t
  "Whether to rename files before copying them to the library"
  :type 'boolean
  :group 'org-bib)


(defcustom org-bib-view-mode-default 'preview
  "Initial view mode."
  :type '(radio (const :tag "None"        none)
                (const :tag "Bibtex"      bibtex)
                (const :tag "Preview"     preview)
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


(defvar org-bib--refile-history '()
  "Dedicated refile history")


(defvar org-bib--view-mode-current org-bib-view-mode-default
  "Current view mode")


(defvar org-bib--author-name-csl
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<style xmlns=\"http://purl.org/net/xbiblio/csl\" class=\"in-text\" version=\"1.0\" demote-non-dropping-particle=\"never\">
  <macro name=\"author-bib\">
    <names variable=\"composer\" delimiter=\", \">
      <name name-as-sort-order=\"all\" and=\"symbol\" sort-separator=\", \" initialize-with=\". \" delimiter=\", \" delimiter-precedes-last=\"always\"/>
      <substitute>
        <names variable=\"author\"/>
        <names variable=\"illustrator\"/>
        <names variable=\"director\">
          <name name-as-sort-order=\"all\" and=\"symbol\" sort-separator=\", \" initialize-with=\". \" delimiter=\", \" delimiter-precedes-last=\"always\"/>
          <label form=\"long\" prefix=\" (\" suffix=\")\" text-case=\"title\"/>
        </names>
        <names variable=\"editor\" delimiter=\", \">
          <name name-as-sort-order=\"all\" and=\"symbol\" sort-separator=\", \" initialize-with=\". \" delimiter=\", \" delimiter-precedes-last=\"always\"/>
          <label form=\"short\" prefix=\" (\" suffix=\")\" text-case=\"title\"/>
        </names>
        <names variable=\"editorial-director\">
          <name name-as-sort-order=\"all\" and=\"symbol\" sort-separator=\", \" initialize-with=\". \" delimiter=\", \" delimiter-precedes-last=\"always\"/>
          <label form=\"short\" prefix=\" (\" suffix=\")\" text-case=\"title\"/>
        </names>
        <names variable=\"collection-editor\">
          <name name-as-sort-order=\"all\" and=\"symbol\" sort-separator=\", \" initialize-with=\". \" delimiter=\", \" delimiter-precedes-last=\"always\"/>
          <label form=\"short\" prefix=\" (\" suffix=\")\" text-case=\"title\"/>
        </names>
      </substitute>
    </names>
  </macro>
  <bibliography >
     <sort>
       <key macro=\"author-bib\"/>
     </sort>
     <layout>
       <text macro=\"author-bib\" />
     </layout>
   </bibliography>
</style>"
  "CSL style to get only author names (as used in preview mode).")

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
         (org-filename (buffer-file-name))
         (library-path (or (cadar (org-collect-keywords '("LIBRARY-PATH")))
                           (file-name-directory org-filename)))
         (target-file (cond ((not org-bib-library-copy-file)
                             source-file)
                            ((not org-bib-library-rename-file)
                             (concat (file-name-as-directory library-path)
                                     (file-name-nondirectory source-file)))
                            (t
                             (concat (file-name-as-directory library-path)
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
  "Set view mode, one of 'info, 'abstract, 'notes, 'pdf, 'bibtex,
'url, 'preview or 'none."

  (interactive)
  ;; (when (member mode '(pdf url info abstract notes preview bibtex none))
  (org-bib-view mode)
  (setq org-bib--view-mode-current mode)
  (force-mode-line-update))


(defun org-bib-view (&optional mode)
  "View selected entry according to mode, one of 'info, 'abstract,
'notes, 'pdf, 'bibtex or 'none"
  
  (interactive)
  (let ((mode (or mode org-bib--view-mode-current)))
    (unless (eq mode 'none)
      (save-selected-window
        (save-excursion
          (org-bib-goto)
          (cond ((eq mode 'abstract) (org-bib-view-abstract))
                ((eq mode 'notes)    (org-bib-view-notes))
                ((eq mode 'pdf)      (org-bib-view-pdf))
                ((eq mode 'url)      (org-bib-view-url))
                ((eq mode 'preview)  (org-bib-view-preview))
                ((eq mode 'bibtex)   (org-bib-view-bibtex))
                (t                   nil)))))))

(defun org-bib-view-goto (&optional mode)
  "View and go to selected entry according to mode, one of 'info, 'abstract,
'notes, 'pdf, 'bibtex or 'none"

  (org-bib-view mode)
  (other-window 1))
  

(defun org-bib-goto ()
  "Move point to the corresponding current item in the org buffer."

  (interactive)
  (let ((entry (imenu-list--find-entry)))
    (when (imenu--subalist-p entry)
      (setq entry (cons (car entry)
                        (get-text-property 0 'marker (car entry)))))
    (imenu-list--goto-entry entry)
    (org-hide-drawer-all)))


(defun org-bib-view-abstract ()
  "Narrow to abstract heading r for the current item."
  
  (org-goto-first-child)
  (org-narrow-to-subtree)
  (goto-char (+ 2 (org-element-property :begin (org-element-at-point)))))


(defun org-bib-view-notes ()
  "Narrow to notes heading for the current item."
  
  (org-goto-first-child)
  (org-goto-sibling)
  (org-narrow-to-subtree))
  

(defun org-bib--author-names ()
  "Return author names from the bibtex entry at point. Author names
are rendered through citeproc, using a dedicated csl style that
only render author names."
 
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((data (citeproc-bt-entry-to-csl (bibtex-parse-entry)))
           (proc (citeproc-create-style
                  org-bib--author-name-csl (org-cite-csl--locale-getter) "en-US" "en-US")))
      (citeproc-render-item data proc 'bib 'plain))))

(defun org-bib--preview (&optional with-abstract with-note)
  "Build a preview of the entry at point."
  
  (let* ((author (progn
                   (org-bibtex-export-to-kill-ring)
                   (with-temp-buffer
                     (yank)
                     (goto-char (point-min))
                     (org-bib--author-names))))
         (title     (org-entry-get (point) "TITLE"))
         (doi       (org-entry-get (point) "DOI"))
         (custom-id (org-entry-get (point) "CUSTOM_ID"))
         (year      (org-entry-get (point) "YEAR"))
         (filename  (buffer-file-name imenu-list--displayed-buffer))
         (journal   (or (org-entry-get (point) "JOURNAL")
                       (org-entry-get (point) "CONFERENCE")
                       (if (string= (org-entry-get (point) "BTYPE") "book") "BOOK")
                       (if (member "PREPRINT" (org-get-tags)) "PREPRINT")
                       "ONLINE"))
         (abstract (when with-abstract
                     (save-excursion
                       (org-goto-first-child)
                       (let* ((element (org-element-at-point))
                              (beg (org-element-property :robust-begin element))
                              (end (org-element-property :robust-end element)))
                         (if (and beg end)
                             (buffer-substring-no-properties beg (+ end 1)))))))
         (abstract (if abstract
                       (string-trim abstract)
                     "None."))
         
         (notes (when with-note
                  (save-excursion
                    (org-goto-first-child)
                    (org-goto-sibling)
                    (let* ((element (org-element-at-point))
                           (beg (org-element-property :robust-begin element))
                           (end (org-element-property :robust-end element)))
                      (if (and beg end)
                          (buffer-substring-no-properties beg (+ end 1)))))))
         (notes (if notes
                    (string-trim notes)
                  "None.")))

    (concat
     (propertize (format "[[file:%s::#%s:abstract][%s (%s)]]\n"
                         filename  custom-id (upcase journal) year)
                 'face '(:inherit (nano-popout nano-strong) :height 0.85))
     (propertize (format "*%s*\n" title))
     (propertize (format "/%s/\n\n" author) 'face '(:inherit default :height 0.85))
     (when with-abstract
       (concat
        (propertize "*Abstract.* " 'face 'bold)
        (if abstract
            (propertize abstract 'face '(:inherit nano-default))
          "")
        "\n\n"))
     (when with-note
       (concat
        (propertize "*Notes.* " 'face 'bold)
        (if notes
            (propertize notes 'face '(:inherit nano-default))
          "")
        "\n")))))


(defun org-bib-view-preview ()
  "View preview of current item in a dedicated preview buffer."
  
  (let ((preview))
    (save-excursion
      (if (and (eq 1 (org-element-property :level (org-element-at-point)))
               (org-goto-first-child))
          (let ((sibling t))
            (while sibling
              (when (org-bib--item-visible)
                (setq preview (concat preview (org-bib--preview))))
              (setq sibling (org-goto-sibling))))
        (setq preview (org-bib--preview t t))))
  
    (switch-to-buffer (get-buffer-create "*org-bib: preview*"))
    (let ((inhibit-read-only t))
      (delete-region (point-min) (point-max))
      (insert preview)
      (visual-line-mode)
      (hl-line-mode)
      (org-mode)
      (setq-local cursor-type nil)
      (setq org-image-actual-width (list (window-width nil t)))
      (setq-local org-return-follows-link t)
      (setq buffer-read-only t)
      (setq header-line-format nil)
      (goto-char (point-min)))))
  

(defun org-bib--bibtex ()
  "Return the bibtex item corresponding to (org) heading at point."
  
  (let ((inhibit-message t))
    (org-bibtex-export-to-kill-ring)
    (with-temp-buffer
      (yank)
      (bibtex-mode)
      (buffer-substring-no-properties (point-min) (point-max)))))


(defun org-bib--item-visible ()
  "Test if heading at point is visible in imenu"
  
  (let* ((element (org-element-at-point))
         (begin (org-element-property :begin element))
         (end (org-element-property :end element))
         (marker (copy-marker begin))
         (level (org-element-property :level element))
         (todo (org-element-property :todo-keyword element))
         (tags (save-excursion
                 (goto-char begin)
                 (org-get-tags)))
         (node (org-imenu-filter-format element todo tags marker level)))
    (imenu--in-alist node imenu-list--imenu-entries)))
    
(defun org-bib-view-bibtex ()
  "View bibtex of current item in a dedicated buffer."

  (let ((content ""))
    (save-excursion
      (if (and (eq 1 (org-element-property :level (org-element-at-point)))
               (org-goto-first-child))
          (let ((sibling t))
            (while sibling
              (when (org-bib--item-visible)
                (setq content (concat content (org-bib--bibtex) "\n")))
              (setq sibling (org-goto-sibling))))
        (setq content (org-bib--bibtex))))
    (switch-to-buffer (get-buffer-create "*org-bib: bibtex*"))
    (erase-buffer)
    (insert content)
    (goto-char (point-min))
    (bibtex-mode)
    (bibtex-reformat)))


(defun org-bib-view-pdf ()
  "View PDF of current item in a dedicated buffer."
  
  (when (org-entry-get (point) "FILENAME")
    (find-file (org-entry-get (point) "FILENAME"))
    (setq-local header-line-format nil)))


(defun org-bib-view-url ()
  "View URL of current item (using xwidgets) in a dedicated buffer."
  
  (when (org-entry-get (point) "URL")
    (xwidget-webkit-goto-url (org-entry-get (point) "URL"))
    (switch-to-buffer (xwidget-buffer (xwidget-webkit-current-session)))
    (setq-local header-line-format nil)))


(defun org-bib-mark-read ()
  "Mark selected entry as read."
  
  (interactive)
  (save-selected-window
    (save-excursion
      (org-bib-goto)
      (org-entry-put (point) "STATUS" "READ")))
  (org-imenu-update))


(defun org-bib-mark-read-and-goto-next ()
  "Mark selected entry as read."
  
  (interactive)
  (org-bib-mark-read)
  (org-bib-next))


(defun org-bib-mark-unread ()
  "Mark selected imenu-entry as read."
  
  (interactive)
  (save-selected-window
    (save-excursion
      (org-bib-goto)
      (org-entry-put (point) "STATUS" "UNREAD")))
  (org-imenu-update))


(defun org-bib-mark-unread-and-goto-next ()
  "Mark selected imenu-entry as read."
  
  (interactive)
  (org-bib-mark-unread)
  (org-bib-next))


(defun org-bib-prev (&optional count)
  "Move to previous entry."
  
  (interactive)
  (with-current-buffer "*Ilist*"
    (previous-line count))
  (org-bib-view))


(defun org-bib-next (&optional count)
  "Move to next entry."
    
  (interactive)
  (with-current-buffer "*Ilist*"
    (next-line count))
  (org-bib-view))


(defun org-bib-pdf-next-page ()
  "Go to next page if a PDF is displayed"

  (interactive)
  (save-selected-window
    ;; WARNING: There must be a better way to select the PDF window
    (other-window 1 nil nil)
    (when (derived-mode-p 'pdf-view-mode)
      (pdf-view-next-page-command))))


(defun org-bib-pdf-prev-page ()
  "Go to previous page if a PDF is displayed"
  
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


(defun org-bib-move ()
  "Move entry to another section"

  (interactive)
  (save-selected-window
    (org-bib-goto)
    (setq-local org-refile-targets `( (,(buffer-name) :maxlevel . 1)))
    (setq-local org-agenda-files (list (buffer-name)))
    (let ((org-refile-history org-bib--refile-history))
      (with-current-buffer imenu-list--displayed-buffer
        (org-refile)
        (setq org-bib--refile-history org-refile-history)
        (org-imenu-update))
      (org-bib-goto)
      (org-bib-view))))


(defun org-bib-move-down ()
  "Move current item down"

  (interactive)
  (let ((start (window-start)))
    (save-selected-window
      (org-bib-goto)
      (org-with-wide-buffer
       (org-move-subtree-down)))
    (org-imenu-update)
    (org-bib-next 1)
    (set-window-start (selected-window) start)))


(defun org-bib-move-up ()
  "Move current item up"

  (interactive)
  (let ((start (window-start)))
  (save-selected-window
    (org-bib-goto)
    (org-with-wide-buffer
     (org-move-subtree-up)))
  (org-imenu-update)
  (org-bib-prev 2)
  (set-window-start (selected-window) start)))
  

(defun org-bib-tag ()
  "Tag current entry "

  (interactive)
  (save-selected-window
    (org-bib-goto)
    (org-set-tags-command)
    (org-imenu-update)))


(defun org-bib-export ()
  "Export library to a bibtex file"
  
  (interactive)
  (save-selected-window
    (save-excursion
      (with-current-buffer imenu-list--displayed-buffer
        (let* ((filename (buffer-file-name))
               (library-path (or (cadar (org-collect-keywords '("LIBRARY-PATH")))
                                 (file-name-directory filename)))
              (library-file (or (cadar (org-collect-keywords '("LIBRARY-FILE")))
                                (file-name-with-extension filename "bib"))))
          (org-bibtex library-file))))))


(defun org-bib--imenu-filter-format (oldfun element todo tags marker level)
  "Advice function that is used to modify face for unread entries."
  
  (let* ((status (org-element-property ':STATUS element))
         (entry (apply oldfun (list element todo tags marker level))))
    (if (and (string= status "UNREAD") (= level 2))
        (add-face-text-property 0 (length entry)
                                '(:inherit nano-salient) nil entry))
    entry))


(defun org-bib-activate ()
  "Activates org-bib mode."

  (setq org-bibtex-headline-format-function #'org-bib-headline-format)
  (setq pdf-drop-search-hook #'org-bib-pdf-process)
  (setq imenu-list-after-jump-hook #'org-tree-to-indirect-buffer)
  (advice-add 'org-imenu-filter-format :around #'org-bib--imenu-filter-format)

  ;; BUG: if bibtex-mode is initialized, bibtex-parse-entry does not work
  ;; properly
  (with-current-buffer (get-buffer-create "__dummy__.bib")
    (bibtex-mode)
    (kill-buffer))
  
  (with-current-buffer (current-buffer)
    (pdf-drop-mode)
    (org-imenu))

  (with-current-buffer "*Ilist*"
    (org-imenu-update)
    (pdf-drop-mode)
    (org-bib-view)

    ;; This prevents the default behavior to override our mouse binding
    (setq-local mouse-1-click-follows-link nil)
    
    (let ((map (current-local-map)))
      (define-key map (kbd "<down-mouse-1>") #'org-bib-view)
      (define-key map (kbd "SPC") #'(lambda () (interactive) (org-bib-view 'info)))
      (define-key map (kbd "RET") #'org-bib-goto)
      
      (define-key map (kbd "<down>") #'org-bib-next)
      (define-key map (kbd "<up>")   #'org-bib-prev)

      (define-key map (kbd "M-<down>") #'org-bib-move-down)
      (define-key map (kbd "M-<up>")   #'org-bib-move-up)

      (define-key map (kbd "<left>")   #'org-bib-pdf-prev-page)
      (define-key map (kbd "<right>")  #'org-bib-pdf-next-page)

      (define-key map (kbd "!") #'org-bib-mark-read-and-goto-next)
      (define-key map (kbd "?") #'org-bib-mark-unread-and-goto-next)

      (define-key map (kbd "m") #'org-bib-move)
      (define-key map (kbd "e") #'org-bib-export)
      (define-key map (kbd "t") #'org-bib-tag)
      
      (define-key map (kbd "p") #'(lambda () (interactive) (org-bib-view 'pdf)))
      (define-key map (kbd "u") #'(lambda () (interactive) (org-bib-view 'url)))
      (define-key map (kbd "i") #'(lambda () (interactive) (org-bib-view 'info)))
      (define-key map (kbd "n") #'(lambda () (interactive) (org-bib-view 'notes)))
      (define-key map (kbd "b") #'(lambda () (interactive) (org-bib-view 'bibtex)))
      (define-key map (kbd "=") #'(lambda () (interactive) (org-bib-view 'preview)))
      (define-key map (kbd "a") #'(lambda () (interactive) (org-bib-view 'abstract)))

      (define-key map (kbd "P") #'(lambda () (interactive) (org-bib-view-goto 'pdf)))
      (define-key map (kbd "U") #'(lambda () (interactive) (org-bib-view-goto 'url)))
      (define-key map (kbd "I") #'(lambda () (interactive) (org-bib-view-goto 'info)))
      (define-key map (kbd "N") #'(lambda () (interactive) (org-bib-view-goto 'notes)))
      (define-key map (kbd "A") #'(lambda () (interactive) (org-bib-view-goto 'abstract)))

      (define-key map (kbd "v p")   #'(lambda () (interactive) (org-bib-view-mode 'pdf)))
      (define-key map (kbd "v RET") #'(lambda () (interactive) (org-bib-view-mode 'none)))
      (define-key map (kbd "v i")   #'(lambda () (interactive) (org-bib-view-mode 'info)))
      (define-key map (kbd "v n")   #'(lambda () (interactive) (org-bib-view-mode 'notes)))
      (define-key map (kbd "v b")   #'(lambda () (interactive) (org-bib-view-mode 'bibtex)))
      (define-key map (kbd "v =")   #'(lambda () (interactive) (org-bib-view-mode 'preview)))
      (define-key map (kbd "v a")   #'(lambda () (interactive) (org-bib-view-mode 'abstract))))))


(defun org-bib-inactivate ()
  "Inactivates org-bib mode."

  ;; Restore default org-bibtex headline format
  (setq org-bibtex-headline-format-function #'(lambda (entry) (cdr (assq :title entry))))

  ;; Remove our special filter
  (advice-remove 'org-imenu-filter-format #'org-bib--imenu-filter-format)

  ;; Remove our hook on pdf drop
  (setq pdf-drop-search-hook nil)

  ;; Deactivate pdf-drop mode on org buffer and widen it
  (with-current-buffer imenu-list--displayed-buffer
    (pdf-drop-mode -1)
    (widen))

  ;; Quit org-imenu
  (org-imenu-quit))



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
