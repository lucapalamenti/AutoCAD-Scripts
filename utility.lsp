
(vl-load-com)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PAGE SETUP FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Configures the Layout
(defun c:SetupLayout (/ doc lay)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

  ; Make sure we're not in the "Model" tab (different than MODEL space)
  ; Find the layout tab at index 1 and set the selected tab to it
  (vlax-for lay (vla-get-Layouts doc)
    (if (= (vla-get-TabOrder lay) 1)
      (setvar "CTAB" (vla-get-Name lay))
    )
  )

  (setq lay (vla-get-ActiveLayout doc))
  (vla-RefreshPlotDeviceInfo lay)
  ; Set Printer/plotter name
  (vla-put-ConfigName lay "AutoCAD PDF (General Documentation).pc3")
  ; Set Paper size
  (vla-put-CanonicalMediaName lay "ANSI_full_bleed_B_(17.00_x_11.00_Inches)")
  ; Set Drawing orientation to Landscape
  (vla-put-PlotRotation lay ac0degrees)
  ; Set plot window bounds to [(0,0), (17,11)]
  (vla-SetWindowToPlot lay
    ; Bottom Left coordinate
    (vlax-make-variant
      (vlax-safearray-fill
        ; Create a safearray of 2 doubles (indices 0 to 1 inclusive)
        (vlax-make-safearray vlax-vbDouble '(0 . 1)) '(0.0 0.0)
      )
    )
    ; Top Right coordinate
    (vlax-make-variant
      (vlax-safearray-fill
        ; Create a safearray of 2 doubles (indices 0 to 1 inclusive)
        (vlax-make-safearray vlax-vbDouble '(0 . 1)) '(17.0 11.0)
      )
    )
  )
  ; Set "What to plot:" to 4 (Window)
  (vla-put-PlotType lay 4)
  ; Set Plot Offset to X: 0, Y: 0
  (vla-put-PlotOrigin lay (vlax-make-variant
    (vlax-safearray-fill
      ; Create a safearray of 2 doubles (indices 0 to 1 inclusive)
      (vlax-make-safearray vlax-vbDouble '(0 . 1)) '(0.0 0.0)
    )
  ))

  (princ)
)

;; Make sure the VIEWPORT is ordered, centered, & sized correctly
(defun c:FixViewport (/ ss ent obj)
  ; Select all viewport objects in the drawing (there should only be 1)
  (if (setq ss (ssget "_X" '((0 . "VIEWPORT"))))
    (progn
      ; Move VIEWPORT to the front so its above the title block
      (vl-cmdf "_.DRAWORDER" ss "" "F")

      ; Get the first (and only) viewport
      (setq ent (ssname ss 0))
      (setq obj (vlax-ename->vla-object ent))

      (vla-put-DisplayLocked obj :vlax-true)

      ; Create a center point with X=8.5, Y=5.5
      (vla-put-Center obj (vlax-3d-point 8.5 5.5 0))
      (vla-put-Height obj 10.5)
      (vla-put-Width obj 16.5)

      (princ "Viewport properties updated: Center (8.5, 5.5), Height 10.5, Width 16.5, and moved to front.\n")
    )
    (princ "No viewport found in the drawing.\n")
  )

  (princ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BLOCK REFERENCE FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Replaces and copies all attributes from one block reference to a new one
; @param newEntFilePath [String] - File path to a .dwg file that contains a block reference
(defun ReplaceBlockReference ( oldEnt newEntFilePath / newEnt )
  (command "_.PSPACE")
  
  (InsertBlock newEntFilePath)
  (setq newEnt (entlast))
  (CopyBlockAttributes oldEnt newEnt)
  (entdel oldEnt)

  (command "_.MSPACE")
  (princ "Replaced Block Reference\n")
)

; INSERTs a block reference onto the drawing
(defun InsertBlock (filepath)
  (setvar "ATTREQ" 0)
  (command
    "_.-INSERT"
    filepath
    '(0 0 0) ; insertion point
    1 ; X scale
    1 ; Y scale
    0 ; rotation
  )
  (setvar "ATTREQ" 1)
  (princ)
)

; Copies attribute values from one block reference to another
; @param oldEnt [ENAME] - The entity name of the source block reference
; @param newEnt [ENAME] - The entity name of the target block reference
(defun CopyBlockAttributes (oldEnt newEnt / oldObj newObj oldMap val)
  (vl-load-com)

  (setq oldObj (vlax-ename->vla-object oldEnt)
        newObj (vlax-ename->vla-object newEnt))
  ;; Build a TAG -> VALUE map for old block
  (setq oldMap nil)

  (foreach att (vlax-invoke oldObj 'GetAttributes)
    (setq oldMap
      (cons
        (cons (strcase (vla-get-TagString att))
              (vla-get-TextString att))
        oldMap
      )
    )
  )
  
  ;; Apply values of map to matching tags in new block
  (foreach att (vlax-invoke newObj 'GetAttributes)
    (if (setq val
        (cdr
          (assoc
            (strcase (vla-get-TagString att))
            oldMap
          )
        )
      )
      (vla-put-TextString att val)
    )
  )

  (vla-Update newObj)
  (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TEXT FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Converts all TEXT objects to MTEXT
(defun c:Text2MText (/ ss)
  (setq ss (ssget "_X" '((0 . "TEXT"))))
  (sssetfirst nil ss)
  (if ss
    (progn
      (command "_.TXT2MTXT" ss "")
      (princ "\nAll TEXT objects converted to MTEXT.")
    )
    (princ "\nNo TEXT objects found in the drawing.")
  )
  (princ)
)

;; Runs all text masking functions
(defun c:MaskText (/)
  (MaskAllMText)
  (MaskAllMLeaders)
  (c:DeleteWipeouts)
  (princ)
)
;; Applies a background color mask to all MTEXT objects
(defun MaskAllMText (/ ss i dxf_ent)
  (if (setq ss (ssget "_X" '((0 . "MTEXT"))))
    (repeat (setq i (sslength ss))
      (setq dxf_ent (entget (ssname ss (setq i (1- i)))))
      ; Remove old DXF codes (90, 63, 45, 40, 41) if they exist 
      (setq dxf_ent (vl-remove-if '(lambda (x) (member (car x) '(90 63 45 40 41))) dxf_ent))
      ; Add new DXF codes for background fill, color, padding, and width
      ; 90 = Background fill flag
      ; 63 = Background color
      ; 45 = Text padding
      ; 40 = Width
      ; 41 = Height
      (setq dxf_ent (append dxf_ent '((90 . 3) (63 . 256) (45 . 1.5) (41 . 0))))
      (entmod dxf_ent)
      (entupd (ssname ss i))
    )
    (princ "\nNo MText objects found.")
  )
  (princ)
)
;; Applies a background color mask to all MLEADER objects
(defun MaskAllMLeaders (/ ss i ent obj)
  (if (setq ss (ssget "_X" '((0 . "MULTILEADER"))))
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))

        ; Enable background mask
        (if (vlax-property-available-p obj 'TextBackgroundFill)
          (vla-put-TextBackgroundFill obj :vlax-true)
        )

        (setq i (1+ i))
      )
    )
    (princ "\nNo MLEADER objects found.")
  )
  (princ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UTILITY / DEBUGGING FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; prints out DXF codes for a selected object to the command line
; Mainly useful for debugging and understanding how to manipulate object properties with DXF codes
(defun c:ShowDXF (/ e)
  (if (setq e (car (entsel "\nSelect object: ")))
    (foreach x (entget e)
      (print x)
    )
  )
  (princ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MISC FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Returns a list of file names from the provided file
; @param filename [string] - a file path
(defun FileToList (filename / fp result line)
  ; Initialize list of file paths
  (setq result '())

  (if (setq fp (open filename "r"))
    (progn
      (while (setq line (read-line fp))
        (setq result
          (cons
            (strcase (vl-string-trim " " line))
            result
          )
        )
      )
      (close fp)
    )
  )
  (princ (strcat "\nFound " (itoa (length result)) " names in " filename ":\n"))
  (reverse result)
)

; Pauses the script for a given time
; @params secs [Integer] - the number of seconds to pause the script for
(defun WaitSeconds (secs / endTime)
  (setq endTime (+ (getvar "DATE") (/ secs 86400.0)))
  (while (< (getvar "DATE") endTime))
)

;; deletes all WIPEOUT objects
(defun c:DeleteWipeouts (/ ss)
  (if (setq ss (ssget "_X" '((0 . "WIPEOUT"))))
    (command "_.ERASE" ss "")
  )
  (princ)
)
