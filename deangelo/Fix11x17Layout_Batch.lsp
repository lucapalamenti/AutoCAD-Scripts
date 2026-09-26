(vl-load-com)
(load "scripts/GlobalVariables.lsp")

;; ============================================================
;; FIX 11x17 LAYOUT - SINGLE DRAWING + RECURSIVE FOLDER BATCH
;; Commands:
;;   FIX11X17
;;   FIX11X17FOLDER
;;
;; FIX11X17FOLDER uses the same proven folder method as the user's
;; UpdateRevisionDescription routine: (getstring T ...) followed by
;; recursive vl-directory-files scanning.
;;
;; Progress log:
;;   Fix11x17_BatchLog.txt in selected root folder.
;;   STARTED is written before opening each drawing.
;;   COMPLETED is written only after successful save and close.
;; ============================================================

(defun F11-NormalizeFolder (folder)
  (setq folder (vl-string-trim "\"" folder))
  (while (and (> (strlen folder) 3)
              (= (substr folder (strlen folder) 1) "\\"))
    (setq folder (substr folder 1 (1- (strlen folder)))))
  folder
)

(defun F11-GetAllDwgs (folder / result item fullpath)
  (setq result '())
  ;; Find DWGs in current folder.
  (foreach item (vl-directory-files folder "*.dwg" 1)
    (setq result (cons (strcat folder "\\" item) result)))
  ;; Search subfolders recursively.
  (foreach item (vl-directory-files folder nil -1)
    (if (and (/= item ".") (/= item ".."))
      (progn
        (setq fullpath (strcat folder "\\" item))
        (setq result (append result (F11-GetAllDwgs fullpath))))))
  result
)

(defun F11-Log (msg / f)
  ;; Open/write/close every line so completed progress is on disk even
  ;; if AutoCAD crashes later in the run.
  (if (setq f (open *F11-LogPath* "a"))
    (progn
      (write-line msg f)
      (close f)))
)

(defun F11-Timestamp ()
  (menucmd "M=$(edtime,$(getvar,date),YYYY-MM-DD HH:MM:SS)")
)

(defun F11-FindFullBleedB (layout / mediaList media locale result raw)
  (vla-RefreshPlotDeviceInfo layout)
  (setq raw (vla-GetCanonicalMediaNames layout))
  (setq mediaList
    (cond
      ((= (type raw) 'VARIANT)
       (vlax-safearray->list (vlax-variant-value raw)))
      ((= (type raw) 'SAFEARRAY)
       (vlax-safearray->list raw))
      (T raw)))
  (foreach media mediaList
    (setq locale
      (vl-catch-all-apply 'vla-GetLocaleMediaName (list layout media)))
    (if (and (not (vl-catch-all-error-p locale))
             (= (type locale) 'STR)
             (wcmatch (strcase locale) "*ANSI*FULL*BLEED*B*"))
      (setq result media)))
  result
)

(defun F11-FindMainViewport (layout / blk obj center coords x y width height score bestScore bestVP)
  (setq blk (vla-get-Block layout)
        bestScore nil
        bestVP nil)
  ;; Choose the viewport closest to the desired sheet viewport:
  ;; center 8.5,5.5 and size 17x11.
  (vlax-for obj blk
    (if (and (= (vla-get-ObjectName obj) "AcDbViewport")
             (vlax-property-available-p obj 'Center)
             (vlax-property-available-p obj 'Width)
             (vlax-property-available-p obj 'Height)
             (< (vla-get-Width obj) 1000.0)
             (< (vla-get-Height obj) 1000.0))
      (progn
        (setq center (vlax-variant-value (vla-get-Center obj)))
        (setq coords (vlax-safearray->list center))
        (setq x (nth 0 coords)
              y (nth 1 coords)
              width (vla-get-Width obj)
              height (vla-get-Height obj))
        (setq score
          (+ (abs (- x 8.5))
             (abs (- y 5.5))
             (abs (- width 17.0))
             (abs (- height 11.0))))
        (if (or (null bestScore) (< score bestScore))
          (setq bestScore score
                bestVP obj)))))
  bestVP
)
(defun F11-GetTargetLayout (doc / layouts lay result)
  (setq layouts (vla-get-Layouts doc)
        result nil)
  ;; Prefer Layout1.
  (vlax-for lay layouts
    (if (= (strcase (vla-get-Name lay)) "LAYOUT1")
      (setq result lay)))
  ;; Fallback to first non-Model layout.
  (if (null result)
    (vlax-for lay layouts
      (if (and (null result)
               (/= (strcase (vla-get-Name lay)) "MODEL"))
        (setq result lay))))
  result
)

(defun F11-FixDocument (doc / layout media vp center result plotOrigin)
  (setq layout (F11-GetTargetLayout doc))
  (if (null layout)
    "ERROR: No paper-space layout found."
    (progn
      ;; Printer/plotter.
      (vla-RefreshPlotDeviceInfo layout)
      (setq result
        (vl-catch-all-apply
          'vla-put-ConfigName
          (list layout "AutoCAD PDF (General Documentation).pc3")))
      (if (vl-catch-all-error-p result)
        (strcat "ERROR: Could not set plotter: "
                (vl-catch-all-error-message result))
        (progn
          (vla-RefreshPlotDeviceInfo layout)
          ;; Paper size.
          (setq media (F11-FindFullBleedB layout))
          (if (null media)
            "ERROR: ANSI Full Bleed B was not found for the PDF plotter."
            (progn
              (vla-put-CanonicalMediaName layout media)
              ;; Plot settings.
              (vla-put-PlotType layout 5)
              (vla-put-UseStandardScale layout :vlax-true)
              (vla-put-StandardScale layout 0)
              (vla-put-PlotRotation layout 0)
              (setq plotOrigin
                (vlax-make-safearray vlax-vbDouble '(0 . 1)))
              (vlax-safearray-fill plotOrigin '(0.0 0.0))
              (vla-put-PlotOrigin layout plotOrigin)

              ;; Main viewport.
              (setq vp (F11-FindMainViewport layout))
              (if (null vp)
                "ERROR: No main paper-space viewport found."
                (progn
                  ;; Unlock before changing viewport geometry/scale.
                  (if (vlax-property-available-p vp 'DisplayLocked T)
                    (vla-put-DisplayLocked vp :vlax-false))
                  (vla-put-Layer vp "0")
                  (vla-put-Width vp 17.0)
                  (vla-put-Height vp 11.0)
                  (setq center (vlax-3d-point '(8.5 5.5 0.0)))
                  (vla-put-Center vp center)
                  ;; Set the actual AutoCAD standard viewport scale: 6 inches = 1 foot.
                  ; (vla-put-StandardScale vp acVp6in_1ft)
                  ;; Lock after geometry and scale are set.
                  (if (vlax-property-available-p vp 'DisplayLocked T)
                    (vla-put-DisplayLocked vp :vlax-true))
                  (vla-Update vp)
                  (vla-Regen doc 1)
                  (strcat "OK: " (vla-get-Name layout))))))))))
)

(defun c:FIX11X17 (/ acad doc answer)
  (vl-load-com)
  (setq acad (vlax-get-acad-object)
        doc (vla-get-ActiveDocument acad))
  (princ "\n--- FIX 11x17 LAYOUT ---")
  (if (= (getvar "TILEMODE") 1)
    (princ "\nERROR: Switch to a paper-space Layout first.")
    (progn
      (setq answer (vl-catch-all-apply 'F11-FixDocument (list doc)))
      (if (vl-catch-all-error-p answer)
        (princ (strcat "\nERROR: " (vl-catch-all-error-message answer)))
        (princ (strcat "\n" answer)))
      (princ "\n--- FIX11X17 COMPLETE ---")))
  (princ)
)

(defun c:FIX11X17FOLDER (/ folder dwgs acad docs file doc answer saveResult closeResult
                           total count completedCount errorCount f)
  (vl-load-com)

  ;; Same folder-input method used by UpdateRevisionDescription.
  (setq folder *BMS_CP_DWGS*)
  (setq folder (F11-NormalizeFolder folder))

  (cond
    ((= folder "")
     (princ "\nNo folder entered. Cancelled."))

    ((not (vl-file-directory-p folder))
     (princ (strcat "\nERROR: Folder does not exist: " folder)))

    (T
     (setq *F11-LogPath* (strcat folder "\\Fix11x17_BatchLog.txt"))

     ;; Start a fresh log for this run.
     (if (setq f (open *F11-LogPath* "w"))
       (progn
         (write-line "FIX11X17 FOLDER BATCH LOG" f)
         (write-line (strcat "Started: " (F11-Timestamp)) f)
         (write-line (strcat "Root: " folder) f)
         (write-line "" f)
         (close f)))

     ;; Same recursive concept as UpdateRevisionDescription.
     (setq dwgs (F11-GetAllDwgs folder))
     (setq total (length dwgs)
           count 0
           completedCount 0
           errorCount 0)

     (princ (strcat "\nFound " (itoa total) " drawing(s)."))
     (F11-Log (strcat "Found drawings: " (itoa total)))

     (setq acad (vlax-get-acad-object)
           docs (vla-get-Documents acad))

     (foreach file dwgs
       (setq count (1+ count)
             doc nil)
       (princ
         (strcat "\n[" (itoa count) "/" (itoa total) "] Updating: " file))
       (F11-Log
         (strcat "STARTED  | " (F11-Timestamp) " | " file))

       ;; Open drawing.
       (setq answer (vl-catch-all-apply 'vla-Open (list docs file)))

       (if (vl-catch-all-error-p answer)
         (progn
           (setq errorCount (1+ errorCount))
           (F11-Log
             (strcat "ERROR    | " (F11-Timestamp) " | OPEN FAILED | "
                     file " | " (vl-catch-all-error-message answer))))
         (progn
           (setq doc answer)
           (setq answer
             (vl-catch-all-apply 'F11-FixDocument (list doc)))

           (if (vl-catch-all-error-p answer)
             (progn
               (setq errorCount (1+ errorCount))
               (F11-Log
                 (strcat "ERROR    | " (F11-Timestamp) " | " file " | "
                         (vl-catch-all-error-message answer)))
               (vl-catch-all-apply 'vla-Close (list doc :vlax-false)))

             (if (wcmatch answer "OK:*")
               (progn
                 ;; Save.
                 (setq saveResult
                   (vl-catch-all-apply 'vla-Save (list doc)))
                 (if (vl-catch-all-error-p saveResult)
                   (progn
                     (setq errorCount (1+ errorCount))
                     (F11-Log
                       (strcat "ERROR    | " (F11-Timestamp)
                               " | SAVE FAILED | " file " | "
                               (vl-catch-all-error-message saveResult)))
                     (vl-catch-all-apply 'vla-Close
                                        (list doc :vlax-false)))
                   (progn
                     ;; Close after successful save.
                     (setq closeResult
                       (vl-catch-all-apply 'vla-Close
                                          (list doc :vlax-false)))
                     (if (vl-catch-all-error-p closeResult)
                       (progn
                         (setq errorCount (1+ errorCount))
                         (F11-Log
                           (strcat "ERROR    | " (F11-Timestamp)
                                   " | CLOSE FAILED AFTER SAVE | " file " | "
                                   (vl-catch-all-error-message closeResult))))
                       (progn
                         ;; Only now mark the drawing confirmed complete.
                         (setq completedCount (1+ completedCount))
                         (F11-Log
                           (strcat "COMPLETED| " (F11-Timestamp)
                                   " | " file)))))))
               (progn
                 (setq errorCount (1+ errorCount))
                 (F11-Log
                   (strcat "ERROR    | " (F11-Timestamp) " | " file
                           " | " answer))
                 (vl-catch-all-apply 'vla-Close
                                    (list doc :vlax-false))))))))

     (F11-Log "")
     (F11-Log (strcat "Finished: " (F11-Timestamp)))
     (F11-Log (strcat "Completed: " (itoa completedCount)))
     (F11-Log (strcat "Errors: " (itoa errorCount)))

     (princ "\n")
     (princ "\n========================================")
     (princ "\nFIX11X17 FOLDER COMPLETE")
     (princ "\n========================================")
     (princ (strcat "\nCompleted: " (itoa completedCount)))
     (princ (strcat "\nErrors: " (itoa errorCount)))
     (princ (strcat "\nLog: " *F11-LogPath*))))
  (princ)
)

(princ "\nFix11x17Layout_Batch loaded. Commands: FIX11X17 and FIX11X17FOLDER")
(princ)
