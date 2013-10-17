;; =====================================================================
;;     usenet: thorsley@ssd.csd.harris.com  USMail: Tom Horsley
;; compuserve: 76505,364                         511 Kingbird Circle
;;      genie: T.HORSLEY                         Delray Beach, FL  33444
;; ======================== Aging: Just say no! ========================
;;
;; This file provides an interactive buffer-list capability.
;; When the function select-buffer is invoked, the minibuffer
;; prompts you for another buffer to select.  The default is the second
;; buffer on the buffer-list.  Also, all the keys that are normally
;; bound to next-line and previous-line are bound to functions that
;; navigate through the buffer list.  Any keys bound to kill-buffer
;; are rebound to a function that will kill the buffer currently
;; named in the minibuffer, then move to the next buffer on the list.
;; This is a faster means of selecting another buffer than buffer-menu
;; is, but with most of the power.
;;
;; Bob Weiner, Motorola, Inc., 4/5/89
;;   Added 'select-buffer-other-window' command bound to {C-x4b}
;;   usually.
;;
;; Bob Weiner, Motorola, Inc., 10/3/91
;;   Eliminated inefficient recursion for computing buffer list.
;;   This eliminated error of passing max-lisp-eval-depth when working
;;     with many buffers.
;;   Added completion to 'select-buffer' so it works like standard
;;     'switch-to-buffer' function.
;;
;; We have gotten to where we use this technique for several of the
;; packages we have written where something prompts for input, each
;; command keeps its own history list so you can quickly cycle through
;; the previous input to just that command.
;;
;; It is very handy to rebind the keys where next line is, so you can
;; continue to use any cursor keys.
;;


;; Next function is copied from a copylefted function:
;; Copyright (C) 1987, 1988 Kyle E. Jones
(defun bs-window-list (&optional mini)
    "Returns a list of Lisp window objects for all Emacs windows.
Optional first arg MINI t means include the minibuffer window
in the list, even if it is not active.  If MINI is neither t
nor nil it means to not count the minibuffer window even if it is active."
      (let* ((first-window (next-window (previous-window (selected-window)) mini))
	      (windows (cons first-window nil))
	       (current-cons windows)
	        (w (next-window first-window mini)))
	    (while (not (eq w first-window))
	            (setq current-cons (setcdr current-cons (cons w nil)))
		          (setq w (next-window w mini)))
	        windows))
;

;;Date: Mon, 30 Dec 91 19:32:47 -0500
;;From: guttman@linus.mitre.org
(defun bs-list-last (arg)
    "return the car of the last cons cell in ARG."
      (let ((l arg))
	    (while (and (consp l)
			(cdr l))
	            (setq l (cdr l)))
	        (if (consp l)
		    (car l)
		        (error "last called with improper list %s" arg))))
;

;

(defvar bs-buffer-select-list-index 1 "Index into buffer-list")

(defvar bs-buffer-select-local-list nil "Local copy of buffer-list")

(defvar bs-buffer-select-minibuffer-map
    (copy-keymap minibuffer-local-completion-map)
      "This is a copy of the minibuffer completion keymap with all the keys that
were bound to next-line now bound to bs-buffer-select-next and all the keys
that were bound to previous-line now bound to bs-buffer-select-prev.")

(mapcar
  (function
     (lambda (keyseq)
           (define-key bs-buffer-select-minibuffer-map keyseq 'bs-buffer-select-prev)))
   (where-is-internal 'previous-line nil nil))

(mapcar
  (function
     (lambda (keyseq)
           (define-key bs-buffer-select-minibuffer-map keyseq 'bs-buffer-select-next)))
     (where-is-internal 'next-line nil nil))

(mapcar
  (function
     (lambda (keyseq)
           (define-key bs-buffer-select-minibuffer-map keyseq 'bs-buffer-select-kill-buf)))
     (where-is-internal 'kill-buffer nil nil))

(defun bs-make-buffer-list (buffer-list)
    "Returns names from BUFFER-LIST excluding those beginning with a space."
      (delq nil (mapcar '(lambda (b)
			          (if (= (aref (buffer-name b) 0) ? ) nil b))
			    buffer-list)))

;;[bkg:19911231.1214EST]
(defun bs-select-buffer (&optional other-window-func)
    "Interactively select or kill buffer using the minibuffer.
Optional argument OTHER-WINDOW-FUNC non-nil means display buffer in another
window.  The default buffer is the second one in the buffer-list. Other
buffers can selected either explicitly, or by using bs-buffer-select-next and
bs-buffer-select-prev.  Keys normally bound to next-line are bound to
bs-buffer-select-next, those normally bound to previous-line are bound to
bs-buffer-select-prev, and those normally bound to kill-buffer are bound to
bs-buffer-select-kill-buf."
       (interactive)
          (let (inpt)
	          (setq inpt
			            (unwind-protect
					               (let ((minibuffer-local-completion-map bs-buffer-select-minibuffer-map))
							  (setq bs-buffer-select-list-index (min (length (bs-window-list)) (length (buffer-list)))
								       bs-buffer-select-local-list (bs-make-buffer-list (buffer-list)))
							   (let* ((default-buffer (buffer-name (other-buffer)))
								  (completed-buffer
								    (completing-read (concat "Switch to buffer"
											       (cond
												   ((equal other-window-func
													      'switch-to-buffer-other-window)
												        " in other window")
												      ((equal other-window-func
													         'find-buffer-other-screen)
												           " other screen"))
											         ": (default "
												   default-buffer
												     ") ")
										       (mapcar '(lambda (buf)
												       (list (buffer-name buf)))
											         bs-buffer-select-local-list)
										         nil nil nil)))
							        (if (string= completed-buffer "")
								           default-buffer
								       completed-buffer)))))
		        (if other-window-func
			      (apply other-window-func (list inpt))
			  (switch-to-buffer inpt))))
;

(defun bs-select-buffer-other-window ()
    "See documentation for 'bs-select-buffer'."
      (interactive)
        (bs-select-buffer 'switch-to-buffer-other-window))

(defun bs-buffer-select-next ()
  "Move to the next buffer on the buffer-list."
     (interactive)
        (erase-buffer)
	   (setq bs-buffer-select-list-index (1+ bs-buffer-select-list-index))
	      (if (>= bs-buffer-select-list-index (length bs-buffer-select-local-list))
		         (setq bs-buffer-select-list-index 0)
		   )
	         (insert (buffer-name (nth bs-buffer-select-list-index bs-buffer-select-local-list))))

(defun bs-buffer-select-prev ()
  "Move to the previous buffer on the buffer-list."
     (interactive)
        (erase-buffer)
	   (setq bs-buffer-select-list-index (1- bs-buffer-select-list-index))
	      (if (< bs-buffer-select-list-index 0)
		         (setq bs-buffer-select-list-index (1- (length bs-buffer-select-local-list)))
		   )
	         (insert (buffer-name
			                (nth bs-buffer-select-list-index bs-buffer-select-local-list))))

(defun bs-buffer-select-kill-buf ()
  "Kill the buffer currently appearing in the minibuffer, then move to
the next buffer on the buffer-list."
     (interactive)
        (let
	          (
		            (mbuf (current-buffer))        ;; Save the minibuffer because
			                                            ;; kill-buffer selects a buffer
			             (kbuf (nth bs-buffer-select-list-index bs-buffer-select-local-list))
				           )
	        (message "Killing buffer %s." (buffer-name kbuf))
		      (kill-buffer kbuf)
		            (set-buffer mbuf))
	   ;; Rebuild the buffer list, so that the killed buffer doesn't appear
	   ;; in it.  Under certain circumstances, the buffer might not have
	   ;; gone away, such as killing "*scratch*" when it is the last buffer.

	   (setq bs-buffer-select-local-list (bs-make-buffer-list (buffer-list)))

	      ;; Fix bs-buffer-select-list-index, in case it went off the end of
	      ;; the list (in either direction, just to be absolutely safe).

	      (if (< bs-buffer-select-list-index 0)
		         (setq bs-buffer-select-list-index (1- (length bs-buffer-select-local-list))))
	         (if (>= bs-buffer-select-list-index (length bs-buffer-select-local-list))
		            (setq bs-buffer-select-list-index 0))
		    (erase-buffer)
		       (insert (buffer-name
				              (nth bs-buffer-select-list-index bs-buffer-select-local-list))))

(provide 'bs-select-buffer)

(defun bs-buffer-next ()
    "Works around a ring of buffers.  Steps to the next buffer in list,
putting the current buffer at the end of the selection list."
      (interactive)
        (bury-buffer (current-buffer))
	  (switch-to-buffer (other-buffer)))

(defun bs-kill-current-buffer ()
    "Kill current buffer -- no prompt"
      (interactive)
        (kill-buffer (get-buffer (current-buffer))))

(defun bs-kill-buffer-and-return ()
    "Kill current buffer and skip back to the other window."
      (interactive)
        (kill-buffer (get-buffer (current-buffer)))
	  (other-window 1))

(defun bs-toggle-buffer ()
    "Toggles the current buffer with the one at the top of the
buffer ring."
      (interactive)
        (switch-to-buffer nil))

(defun bs-toggle-buffer-other-window ()
    "Toggles (restores) the buffer of the other winodw."
      (interactive)
        (let ((curr-window (selected-window)))
	      (other-window 1)
	          (switch-to-buffer nil)
		      (select-window curr-window)))


(defun bs-buffer-previous ()
    (interactive)
      (switch-to-buffer (bs-list-last (buffer-list))))

