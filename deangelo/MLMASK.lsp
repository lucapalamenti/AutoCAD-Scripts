;; MLMASK.lsp
;; Change background-mask border offset factor for selected MULTILEADERs.
;; Workflow: preselect MLEADERs -> run MLMASK -> enter factor (1.0 to 5.0).

(vl-load-com)

(defun MLMASK:set-context-offset (en off / ed out x inctx changed)
  (setq ed (entget en)
        out nil
        inctx nil
        changed nil)
  (foreach x ed
    (cond
      ((and (= (car x) 300) (= (cdr x) "CONTEXT_DATA{"))
       (setq inctx T)
       (setq out (append out (list x))))
      ((and inctx (= (car x) 301) (= (cdr x) "}"))
       (setq inctx nil)
       (setq out (append out (list x))))
      ;; Group 141 inside CONTEXT_DATA is the MLeader MText mask border offset factor.
      ((and inctx (= (car x) 141) (not changed))
       (setq out (append out (list (cons 141 off))))
       (setq changed T))
      (T
       (setq out (append out (list x))))))
  (if changed
    (progn
      (entmod out)
      (entupd en)
      T)
    nil))

(defun c:MLMASK (/ *error* doc undoOpen ss pre i en obj off count failed)
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
        undoOpen nil
        count 0
        failed 0)

  (defun *error* (msg)
    (if undoOpen
      (progn
        (vla-EndUndoMark doc)
        (setq undoOpen nil)))
    (if (and msg (/= msg "Function cancelled") (/= msg "quit / exit abort"))
      (princ (strcat "\nMLMASK error: " msg)))
    (princ))

  ;; Prefer preselected objects, as requested. If none are preselected, prompt.
  (setq pre (ssget "I" '((0 . "MULTILEADER"))))
  (if pre
    (setq ss pre)
    (setq ss (ssget '((0 . "MULTILEADER")))))

  (if (not ss)
    (princ "\nMLMASK: No MULTILEADERs selected.")
    (progn
      (initget 6)
      (setq off (getreal "\nEnter background mask offset factor (1.0 to 5.0): "))
      (cond
        ((not off)
         (princ "\nMLMASK cancelled."))
        ((or (< off 1.0) (> off 5.0))
         (princ "\nMLMASK: Offset must be between 1.0 and 5.0."))
        (T
         (vla-StartUndoMark doc)
         (setq undoOpen T
               i 0)
         (while (< i (sslength ss))
           (setq en (ssname ss i)
                 obj (vlax-ename->vla-object en))

           ;; Ensure the MLeader MText background fill is enabled.
           (vl-catch-all-apply 'vla-put-TextBackgroundFill (list obj :vlax-true))

           (if (MLMASK:set-context-offset en off)
             (setq count (1+ count))
             (setq failed (1+ failed)))
           (setq i (1+ i)))

         (vla-EndUndoMark doc)
         (setq undoOpen nil)
         (command "_.REGEN")
         (princ
           (strcat "\nMLMASK complete. Updated: " (itoa count)
                   " | Failed: " (itoa failed)
                   " | Offset: " (rtos off 2 2)
                   ". One UNDO reverses the change."))))))
  (princ))

(princ "\nMLMASK loaded. Preselect MULTILEADERs, then run MLMASK.")
(princ)
