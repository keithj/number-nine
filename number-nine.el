;;; number-nine.el --- A Scrum project tool. -*- lexical-binding: t; -*-

;; Copyright (C) 2013 Keith James
;;
;; Author: Keith James
;; Keywords: scrum
;;
;; No. 9 is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; No. 9 is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with No. 9.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; "Scrum half: A player nominated to throw the ball into a scrum who
;;  usually wears jersey No. 9." - Laws of the Game Rugby Union.
;;
;; No. 9 (alternatively, number-nine or n9) is an Emacs extension to
;; aid the management of Scrum artifacts such as product and sprint
;; backlogs. The artifacts may be created, manipulated saved, loaded
;; and exported as MultiMarkdown format reports.
;;
;;; Implementation notes:
;;
;; No. 9 function symbols use the package prefix n9- e.g. n9-sprint-number,
;; except in cases where the symbol starts with a verb, in which case the
;; verb is given precedence and -n9- is relegated to an internal position
;; e.g. make-n9-story, find-n9-sprint.
;;
;; Scrum artefacts are represented by simple alists. This implementation
;; detail should be largely invisible to the user because reader and
;; accessor functions are provided for public fields.
;;
;; Artifacts are saved as Emacs Lisp s-expressions (potentially with
;; circular structure) and exported to MultiMarkdown format reports.

(if (>= emacs-major-version 24)
	(require 'cl-lib)
  (require 'cl)
  (defalias 'cl-pairlis 'pairlis)
  (defalias 'cl-find 'find)
  (defalias 'cl-remove-if-not 'remove-if-not))

;;; Code:

(defvar *current-n9-product* nil
  "The current product.")

(defmacro n9-assocdr (key alist &rest args)
  "Returns the cdr of the cons cell returned by calling (assoc KEY ALIST ARGS)."
  `(cdr (assoc ,key ,alist ,@args)))

(defun current-n9-product ()
  "Returns the current product."
  *current-n9-product*)

(defmacro with-n9-product (product &rest body)
  "Executes BODY with *current-n9-product* bound to PRODUCT."
  `(let ((*current-n9-product* ,product))
     ,@body))

(defun* make-n9-product (name owner &key backlog)
  "Returns a new product."
  (cl-pairlis '(name owner stories backlog sprints story-counter sprint-counter)
              (list name owner backlog backlog () 0 0)))

(defun n9-product-name (product)
  "Returns the PRODUCT name."
  (n9-assocdr 'name product))

(defun n9-product-owner (product)
  "Returns the PRODUCT owner."
  (n9-assocdr 'owner product))

(defun n9-story-owners (product)
  "Returns a list of all story owners for PRODUCT."
  (mapcar #'n9-story-owner (n9-product-stories product)))



(defun n9-product-stories (product)
  "Accessor for the list of all stories for PRODUCT."
  (n9-assocdr 'stories product))
(defsetf n9-product-stories (product) (value)
  `(progn
     (unless (listp ,value)
       (error "Expected a list but found %s: %s" (type-of ,value) ,value))
     (setf (n9-assocdr 'stories ,product) ,value)))

(defun n9-product-backlog (product)
  "Accessor for the list of backlog stories for PRODUCT."
  (n9-assocdr 'backlog product))
(defsetf n9-product-backlog (product) (value)
  `(progn
     (unless (listp ,value)
       (error "Expected a list but found %s: %s" (type-of ,value) ,value))
     (setf (n9-assocdr 'backlog ,product) ,value)))

(defun n9-product-sprints (product)
  "Accessor for the list of sprints for PRODUCT."
  (n9-assocdr 'sprints product))
(defsetf n9-product-sprints (product) (value)
  `(progn
     (unless (listp value)
       (error "Expected a list but found %s: %s" (type-of value) value)
       (setf (n9-assocdr 'sprints ,product) ,value))))
  
(defun next-n9-story-number (product)
  "Increment PRODUCT's story number counter and return its new value."
  (incf (n9-assocdr 'story-counter product)))

(defun next-n9-sprint-number (product)
  "Increment PRODUCT's sprint number counter and return its new value."
  (incf (n9-assocdr 'sprint-counter product)))

(defun find-n9-sprint (sprint-number product)
  "Returns sprint SPRINT-NUMBER of PRODUCT."
  (or (cl-find sprint-number (n9-product-sprints product)
               :key #'n9-sprint-number)
      (error "No such story as sprint number %s in product %s"
             sprint-number product)))

(defun find-n9-story (story-number product)
  "Returns story STORY-NUMBER of PRODUCT."
  (or (cl-find story-number (n9-product-stories product) :key #'n9-story-number)
      (error "No such story as story number %s in product %s"
             story-number product)))


(defun* make-n9-sprint (product &key duration number start)
  "Returns a new sprint for PRODUCT."
  (cl-pairlis '(product number start duration)
              (list product
                    (or number (next-n9-sprint-number product))
                    (or start (current-time))
                    (or duration 10))))

(defun n9-sprint-product (sprint)
  "Returns the product to which the SPRINT belongs."
  (n9-assocdr 'product sprint))

(defun n9-sprint-start (sprint)
  "Returns the date the SPRINT started."
  (n9-assocdr 'start sprint))

(defun n9-sprint-number (sprint)
  "Returns the SPRINT number, unique to this SPRINT for this product."
  (n9-assocdr 'number sprint))

(defun n9-sprint-duration (sprint)
  "Returns the total duration of the SPRINT in days."
  (n9-assocdr 'duration sprint))


(defun* make-n9-story (product owner role goal benefit
                            &key criteria cost created number priority)
  "Returns a new story for the PRODUCT."
  (cl-pairlis '(product owner role goal benefit
                        criteria cost created number priority)
              (list product owner role goal benefit
                    (or criteria ())
                    (or cost 0)
                    (or created (current-time))
                    (or number (next-n9-story-number product))
                    (or priority 0))))

(defun n9-story-number (story)
  "Returns the unique number for STORY."
  (n9-assocdr 'number story))

(defun n9-story-product (story)
  "Returns the product of the STORY."
  (n9-assocdr 'product story))

(defun n9-story-owner (story)
  "Accessor for the STORY owner. This is normally the same as the product
owner."
  (n9-assocdr 'owner story))
(defsetf n9-story-owner (story) (value)
  `(progn
     (unless (stringp ,value)
       (error "Expected a string but found %s: %s" (type-of ,value) ,value))
     (setf (n9-assocdr 'owner ,story) ,value)))

(defun n9-story-role (story)
  "Returns the role clause describing the STORY. This indicates the STORY
owner's role e.g. user, developer, development team."
  (n9-assocdr 'role story))

(defun n9-story-goal (story)
  "Returns the goal clause describing the STORY."
  (n9-assocdr 'goal story))

(defun n9-story-benefit (story)
  "Returns the benefit clause describing the STORY."
  (n9-assocdr 'benefit story))

(defun n9-story-criteria (story)
  "Accessor for the list of acceptance criteria for completion of the STORY."
  (n9-assocdr 'criteria story))
(defsetf n9-story-criteria (story) (value)
  `(setf (n9-assocdr 'criteria ,story) ,value))

(defun n9-story-cost (story)
  "Accessor for the estimated cost of the STORY."
  (n9-assocdr 'cost story))
(defsetf n9-story-cost (story) (value)
  `(progn
     (unless (integerp ,value)
       (error "Expected an integer but found %s: %s" (type-of ,value) ,value))
     (setf (n9-assocdr 'cost ,story) ,value)))

(defun n9-story-priority (story)
  "Returns the current priority of the STORY set by the owner."
  (n9-assocdr 'priority story))
(defsetf n9-story-priority (story) (value)
  `(progn
     (unless (integerp ,value)
       (error "Expected an integer but found %s: %s" (type-of ,value) ,value))
     (setf (n9-assocdr 'priority ,story) ,value)))

(defun n9-story-created (story)
  "Returns the date the STORY was created."
  (n9-assocdr 'created story))

(defun n9-story-description (story)
  "Returns a sentence describing the STORY, constructed from its role, goal
and benefit."
  (describe-n9-story (n9-story-role story)
                     (n9-story-goal story)
                     (n9-story-benefit story)))

(defun describe-n9-story (role goal benefit)
  "Returns a sentence constructed from a story its ROLE, GOAL and BENEFIT."
  (format "As a/an %s I/we want %s so that %s" role goal benefit))

(defun filter-n9-story (story &rest keys)
  "Returns an alist containing only the conses of STORY matching KEYS."
  (cl-remove-if-not (lambda (key)
                      (member key keys)) story :key #'car))

(defun n9-iso8601-utc (epoch-time)
  "Returns an ISO 8601 UTC string representation of EPOCH-TIME."
  (format-time-string "%Y-%m-%dT%TZ" epoch-time t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-n9-product (filename)
  "Loads a saved No. 9 product expression from file FILENAME and sets the
current product to its value."
  (interactive "fProduct file name: ")
  (if (file-exists-p filename)
      (let ((print-circle t))
        (with-temp-buffer
          (insert-file-contents filename)
          (setq *current-n9-product* (read (current-buffer)))))
    (setq *current-n9-product* nil)))

(defun save-n9-product (filename)
  "Saves the current No. 9 product to file FILENAME."
  (interactive "FProduct file name: ")
  (let ((product (current-n9-product))
        (print-circle t))
    (message "Saving product %s ..." (n9-product-name product))
    (with-temp-buffer
      (pp (current-n9-product) (current-buffer))
      (write-file filename))
    (message "Saving product %s ...done" (n9-product-name product))))

(defun export-n9-stories (dirname)
  "Exports all of the stories in the current product as MultiMarkdown files,
one per story, into directory DIRNAME. Existing files of the same names will
be overwritten. Files are named

 <product name>-s<story number>.mmd

with all spaces converted to hyphens."
  (interactive "DExport to directory: ")
  (let ((product (current-n9-product)))
    (message "Exporting stories from %s ..." (n9-product-name product))
    (dolist (story (n9-product-stories product))
      (let ((filename (concat (file-name-as-directory dirname)
                              (file-relative-name
                               (format "%s-s%d.mmd"
                                       (replace-regexp-in-string
                                        " " "-" (n9-product-name product))
                                       (n9-story-number story))))))
        (with-temp-buffer
          (princ (mmd-print-story story t) (current-buffer))
          (write-file filename))))
    (message "Exporting %d stories from %s ...done"
             (length (n9-product-stories product))
             (n9-product-name product))))

(defun new-n9-product (name owner)
  "Creates a new No. 9 product and sets it as the current product. The NAME
argument should be short, descriptive name and OWNER should be a string
identifying the product owner, such as an email address."
  (interactive "MProduct name: \nMProduct owner: ")
  (setq *current-n9-product* (make-n9-product name owner)))

(defun show-n9-story (story-number)
  "Displays story STORY-NUMBER from the current product as MultiMarkdown."
  (interactive "nStory number: ")
  (print (mmd-print-story
          (find-n9-story story-number (current-n9-product))) nil))

(defun add-n9-story (role goal benefit)
  "Adds a new story to the current product, prompting 'As a/an <ROLE>
I/we want to <GOAL> so that <BENEFIT>. The story is added to the product story
list, not to the backlog or sprint lists."
  (interactive "MAs a/an: \nMI/we want: \nMso that: ")
  (let ((product (current-n9-product)))
    (push (make-n9-story product (n9-product-owner product) role goal benefit)
          (n9-product-stories product))))

(defun add-n9-story-criterion (story-number criterion)
  "Adds a new acceptance CRITERION to story STORY-NUMBER, which must exist in
the current product."
  (interactive "nStory number: \nMCriterion: ")
  (let ((story (find-n9-story story-number (current-n9-product))))
    (push criterion (n9-story-criteria story))))

(defun prioritise-n9-story (story-number priority)
  "Modifies the priority number of story STORY-NUMBER in the current product to
PRIORITY."
  (interactive "nStory number: \nnNew priority: ")
  (let ((product (current-n9-product)))
    (setf (n9-story-priority (find-n9-story story-number product)) priority)))

(defun transfer-n9-story (story-number owner)
  "Modifies ownership of story STORY-NUMBER in the current product to OWNER."
  (interactive "nStory number: \nMNew owner: ")
  (let ((product (current-n9-product)))
    (setf (n9-story-owner (find-n9-story story-number product)) owner)))


(defmacro define-mmd-header (rank)
  (let ((fname (intern (format "mmd-header-%d" rank)))
        (docstring
         (format "Returns a level %d header as a string, with newline" rank)))
    `(defun ,fname (str)
       ,docstring
       (with-output-to-string
         (dotimes (i ,rank)
           (princ "#"))
         (princ " ")
         (princ str)
         (princ " ")
         (dotimes (i ,rank)
           (princ "#"))
         (princ "\n")))))

(define-mmd-header 1)
(define-mmd-header 2)
(define-mmd-header 3)
(define-mmd-header 4)

(defun mmd-metadata (alist)
  (with-output-to-string
    (dolist (pair alist)
      (princ (format "%s : %s  \n" (car pair) (cdr pair))))
    (princ "\n")))

(defun mmd-simple-link (name url)
  (format "[%s](%s)" name url))

(defun mmd-enum-list (items)
  (with-output-to-string
    (loop
     for item in items
     count item into n
     do (princ (format "%d. %s\n" n item)))))

(defun mmd-print-story (story &optional include-metadata)
  (with-output-to-string
    (when include-metadata
      (princ (format "created : %s\n" (n9-iso8601-utc
                                       (n9-story-created story))))
      (princ (mmd-metadata (filter-n9-story story 'owner 'priority 'cost))))
    (princ (mmd-header-3 (format "Story %d" (n9-story-number story))))
    (princ "\n")
    (princ (n9-story-description story))
    (princ "\n\n")
    (princ (mmd-header-4 "Acceptance criteria"))
    (princ "\n")
    (princ (mmd-enum-list (n9-story-criteria story)))))

(provide 'number-nine)

;;; number-nine.el ends here
