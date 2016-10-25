;;
;; Utilities for working with CIME XML
;;
;; Main 'public' functions:
;;
;; ------------------------------------------------------------------------
;; find-cime-test (run with M-x find-cime-test<ret>)
;;
;; Prompts you to enter a test name in condensed (dotted) format; finds the
;; corresponding line in a testlist xml file (ignoring machine & compiler, since
;; these appear on separate lines in the xml). If a matching test is found,
;; moves the point to the beginning of the matching test; if no match is found,
;; prints an error message.
;;
;; The inclusion of machine_compiler in the search string is optional.
;;
;; If the test has a testmods directory, then the search string must include
;; this testmods directory; however, the search string should use '-' rather
;; than '/' as a directory separator (i.e., the search string should match the
;; standard format of condensed/dotted test names in this respect).
;;
;; If a region is selected, then only searches in the selected region.
;;
;; Examples:
;;
;; - No testmods directory:
;;   ERI_D.T31_g37_rx1.A or ERI_D.T31_g37_rx1.A.hobart_intel will find this line:
;;   <test name="ERI_D" grid="T31_g37_rx1" compset="A">
;;
;; - With testmods directory:
;;   ERR_N3.f19_g16.B1850.allactive-defaultio or
;;   ERR_N3.f19_g16.B1850.yellowstone_intel.allactive-defaultio will find this line:
;;   <test name="ERR_N3" grid="f19_g16" compset="B1850" testmods="allactive/defaultio">
;; ------------------------------------------------------------------------

(defvar test-search-order
  (list (cons (make-symbol "name") t)
        (cons (make-symbol "grid") t)
        (cons (make-symbol "compset") t)
        (cons (make-symbol "mach_comp") nil)
        (cons (make-symbol "testmods") nil))
  "Define the order of items in a search string. cdr is t iff required")

(defun testmods-optional ()
  (let ((opt t))
    (dolist (item test-search-order)
      (if (string= (symbol-name (car item)) "testmods")
          (setq opt (not (cdr item)))))
    opt
    ))

(defun search-prompt ()
  "Construct a search prompt from test-search-order"
  (let* ((sep "")
         (func (lambda (a)
                 (let ((ans (if (cdr a) 
                                (concat sep (symbol-name (car a)))
                              (concat "[" sep (symbol-name (car a)) "]"))))
                   (setq sep ".")
                   ans))))
    (mapconcat func test-search-order "")))

(defun string-value (string)
  "Strip quotes from string"
  (let ((items (split-string string "\"")))
    (if (> (length items) 1)
        (cadr items)
      string)
    ))

(defun testmods-value (string)
  "Strip quotes and convert '/' to '-'"
  (replace-regexp-in-string "/" "-" (string-value string))
  )

(defun parse-test-tag (beg end testmods_optional)
  "Find the name, grid, compset, and testmods entries in a test tag"
  ;; First, split the line along the spaces
  (let* ((test_line (buffer-substring beg end))
         (line_items (split-string test_line "[[:space:]]+"))
         (line_num (save-excursion (goto-char beg) (line-number-at-pos)))
         (name nil)
         (grid nil)
         (compset nil)
         (testmods nil))
    ; Find all the attributes on the test line
    (dolist (item line_items)
      (let ((item_list (split-string item "=")))
        (if (= (length item_list) 2)
            (cond ((string= (car item_list) "name")
                   (setq name (string-value (cadr item_list))))
                  ((string= (car item_list) "grid")
                   (setq grid (string-value (cadr item_list))))
                  ((string= (car item_list) "compset")
                   (setq compset (string-value (cadr item_list))))
                  ((string= (car item_list) "testmods")
                   (setq testmods (testmods-value (cadr item_list))))
                  (t (error (format "Bad test attribute, %s, on line %d"
                                    (car item_list) line_num)))
                  ))))
    (if (not name)
        (error (format "No name attribute on line %d" line_num)))
    (if (not grid)
        (error (format "No grid attribute on line %d" line_num)))
    (if (not compset)
        (error (format "No compset attribute on line %d" line_num)))
    (if (and (not testmods) (not testmods_optional))
        (error (format "No testmods attribute on line %d" line_num)))
    (list name grid compset testmods)
    ))

;; This is not currently used
(defun parse-test (beg end)
  "Find the name, grid, compset, testmods, and machines/compilers for a test"
  ;; First, split the line along the spaces
  (let* ((test_dom (libxml-parse-xml-region beg end))
          (line_num (save-excursion (goto-char beg) (line-number-at-pos)))
         (name nil)
         (grid nil)
         (compset nil)
         (testmods nil))
    ; Find all the attributes on the test line
    ))

(defun find-test-tag (end)
  "Find a CIME <test attr attr ...> tag"
  (let ((retval (re-search-forward "<[[:space:]]*test[[:space:]]+"
                                   end t)))
    (if retval (search-backward "<" (point-min) t))
    retval
    )
  )

(defun find-end-test-tag (end)
  "Find a CIME </test> tag"
  (save-excursion
    (re-search-forward "</[[:space:]]*test[[:space:]]*>" end t)
    ))

(defun testmods-entry-p (str)
  "Return t iff str looks like a testmod entry (two strings separated by
   a hyphen with the first the name of a testmods directory under current
   buffer"
  (let* ((items (split-string  str "-"))
         (num-items (length items))
         (curdir (file-name-directory (buffer-file-name (current-buffer)))))
    (if (eq num-items 2)
        (file-exists-p
         (concat curdir (file-name-as-directory "testmods_dirs")
                 (file-name-as-directory (car items)) (cadr items)))
      nil)
    ))

(defun find-cime-test (test-name)
  "Find a test in a CIME testlist XML file"
  (interactive
   (list (read-string (format "MTest Name (%s): " (search-prompt)) nil 'shist)))
  (let ((search-items (split-string test-name "[.]"))
        (reg-beg (if (use-region-p) (region-beginning) (point-min)))
        (reg-end (if (use-region-p) (region-end) (point-max)))
        (num-req (apply '+ (mapcar (lambda (x) (if (cdr x) 1 0))
                                   test-search-order)))
        (found-test nil)
        (name-pos nil)
        (grid-pos nil)
        (compset-pos nil)
        (testmods-pos nil))
    (if (or (< (length search-items) num-req) 
            (> (length search-items) (length test-search-order)))
        (error (concat "Search string format must be " (search-prompt))))
    ;; Set up the search terms
    (dolist (ts test-search-order)
      (set (car ts) nil))
    (dolist (i (number-sequence 0 (length test-search-order)))
      (cond ((string= (symbol-name (car (nth i test-search-order))) "name")
             (setq name-pos i))
            ((string= (symbol-name (car (nth i test-search-order))) "grid")
             (setq grid-pos i))
            ((string= (symbol-name (car (nth i test-search-order))) "compset")
             (setq compset-pos i))
            ((string= (symbol-name (car (nth i test-search-order))) "testmods")
             (setq testmods-pos i))
            ))
    ;; Process the required arguments
    (dolist (i (number-sequence 0 (1- num-req)))
      (set (car (nth i test-search-order)) (nth i search-items))
      )
    ;; Process any optional arguments
    (dolist (i (number-sequence num-req (1- (length search-items))))
      (if (testmods-entry-p (nth i search-items))
          (set (car (nth testmods-pos test-search-order)) (nth i search-items)))
      )
    ;; Reset search-items to be canonical order (name grid compset testmods)
    (setq search-items
          (mapcar 'eval (list (car (nth name-pos test-search-order))
                              (car (nth grid-pos test-search-order))
                              (car (nth compset-pos test-search-order))
                              (car (nth testmods-pos test-search-order)))))
    ;; Go looking for the test
    (save-excursion
      (goto-char reg-beg)
      (while (and (not found-test) (find-test-tag reg-end))
        (let* ((beg (point))
               (tagend (save-excursion (forward-sexp) (point)))
               (testmods_optional (not (cdr (nth testmods-pos test-search-order))))
               (test_items (parse-test-tag beg tagend testmods_optional)))
          (if (equal search-items test_items)
              (setq found-test beg)
            (goto-char tagend))
          )))
    (if found-test 
        (progn (goto-char found-test) t)
      (message (format "Test %s not found" test-name)))
    ))
