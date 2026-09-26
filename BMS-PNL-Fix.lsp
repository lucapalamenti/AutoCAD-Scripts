;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OVERVIEW
;;; --------
;;; This script automates some of the formatting for BMS-PNL AutoCAD drawings.
;;; Things it does:
;;; - Sets the PAGESETUP to "11x17 vantage layout"
;;; - Replaces the title block with a correctly sized one (including its attributes)
;;; - Ensures the PAPER space has ONLY the Viewport and Title Block, moving all other objects to the MODEL space
;;; - Converts any TEXT objects to MTEXT objects
;;; - CORRECTLY adds a white background to all MTEXT and MLEADER (text with arrow attached) objects and DELETES the previously used WIPEOUT objects
;;; - Formats MTEXT objects:
;;;   - Ensures tags are "ROMANS" font style
;;;   - Sets font size to 24 for tags, and 48 for room labels
;;; - Ensures layering is correct (shop drawing in the back; text, arrows, & shapes in front)
;;; 
;;; ** Any time this file is changed it MUST be reloaded in AutoCAD **
;;; ** Despite the viewport and title block being misaligned from the physical white backround, the drawing will still print/plot correctly **
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IMPORTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(vl-load-com)
(load "scripts/GlobalVariables.lsp")
(load "scripts/utility.lsp")
(load "scripts/TitleBlock.lsp")
(load "scripts/RevHelp.lsp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE SPECIFIC GLOBAL VARIABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MAIN FUNCTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Controls the flow of the script by calling all the other functions in the correct order
(defun c:RUN (/)
  (c:BMSShopBorderFix)
  ; Move all objects to the Model workspace
  (c:MoveObjectsToModel)
  ; Convert all TEXT objects to MTEXT objects
  (c:Text2MText)
  ; Apply a BG mask to all text based objects
  (c:MaskText)
  ; Apply certain text formatting to all MText objects
  (c:FormatMText)
  ; Order objects so that some are in front of others
  (c:OrderObjects)
  ; Deletes random MTEXT with filepath (dont worry about it lol)
  (c:DeletePathMText)

  (command "_.REGEN")

  (princ)
)

;; Fixes mislaigned border
;; ** Run this for drawings that only need a fixed border
(defun c:BorderFix (/)
  ; Configures the page layout
  (c:SetupLayout)
  ; Make sure the paper space is formatted correctly
  (c:FixPaperSpace)

  (princ)
)

(defun c:BMSShopBorderFix (/)
  (c:BorderFix)
  (c:ReplaceBMSShopTitleBlock)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TITLE BLOCK AND SPACE FORMATTING FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Configures the Layout. May take a few seconds to run
(defun c:SetupLayout (/ doc lay)
  (setvar "TILEMODE" 0)
  
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

  ; Make sure we're not in the "Model" tab (different than MODEL space)
  ; Find the layout tab at index 1 and set the selected tab to it
  (vlax-for lay (vla-get-Layouts doc)
    (if (= (vla-get-TabOrder lay) 1)
      (setvar "CTAB" (vla-get-Name lay))
    )
  )

  ; Update page setup to an existing page setup named "11x17 vantage layout"
  ; If it doesn't exist then nothing happens
  ; (command "-pagesetup" "S" "11x17 vantage layout")

  ;; Configure the page setup

  (setq lay (vla-get-ActiveLayout doc))
  (vla-RefreshPlotDeviceInfo lay)
  ; Sets the Printer/plotter name
  (vla-put-ConfigName lay "AutoCAD PDF (General Documentation).pc3")
  ; Paper size
  (vla-put-CanonicalMediaName lay "ANSI_full_bleed_B_(17.00_x_11.00_Inches)")
  ; Landscape orientation
  (vla-put-PlotRotation lay ac0degrees)
  ; Sets the plot window bounds to [(0,0), (17,11)]
  (vla-SetWindowToPlot lay
    ; Bottom Left coordinate
    (vlax-make-variant
      (vlax-safearray-fill
        ; Create a safearray of 2 doubles (indices 0 to 1)
        (vlax-make-safearray vlax-vbDouble '(0 . 1))
        '(0.0 0.0)
      )
    )
    ; Top Right coordinate
    (vlax-make-variant
      (vlax-safearray-fill
        ; Create a safearray of 2 doubles (indices 0 to 1)
        (vlax-make-safearray vlax-vbDouble '(0 . 1))
        '(17.0 11.0)
      )
    )
  )
  ; Sets the "What to plot:" field of the current page setup to 4 (Window)
  (vla-put-PlotType lay 4)
  ; Sets the Plot Offset to X: 0, Y: 0
  (vla-put-PlotOrigin lay (vlax-make-variant
    (vlax-safearray-fill
      ; Create a safearray of 2 doubles (indices 0 to 1)
      (vlax-make-safearray vlax-vbDouble '(0 . 1))
      '(0.0 0.0)
    )
  ))

  (command "_.MSPACE")
  (princ)
)

;; Make sure the paper space is formatted correctly
(defun c:FixPaperSpace (/ ss ent obj insPt center_pt)
  ; Select all viewport objects in the drawing (there should only be 1)
  ; Move to PAPER space and center it
  (if (setq ss (ssget "_X" '((0 . "VIEWPORT"))))
    (progn
      (vl-cmdf "_.CHSPACE" ss "" "P")
      ; Get the first (and only) viewport
      (setq ent (ssname ss 0))
      
      (setq obj (vlax-ename->vla-object ent))
      ; Create a center point with X=8.5, Y=5.5
      (setq center_pt (vlax-3d-point 8.5 5.5 0))
      
      ; Set the viewport properties
      (vla-put-Center obj center_pt)

      (vla-put-Height obj 10.5)
      (vla-put-Width obj 16.5)

      (princ "\nViewport properties updated: Center (8.5, 5.5), Height 11, Width 17.")
    )
    (princ "\nNo viewport found in the drawing.")
  )
  (command "_.MSPACE")
  ; Select block reference object in the drawing (the title block should be the only one)
  ; Move to PAPER space and center it
  (setq ent (GetTitleBlock))
  (setq ss (ssadd ent (ssadd)))
  (if (> (sslength ss) 0)
    (progn
      (command "_.CHSPACE" ss "" "P")
      (setq obj (vlax-ename->vla-object ent))
      (setq center_pt (vlax-3d-point 0 0 0))
      
      ; Set the block reference properties
      ; (vla-put-InsertionPoint obj center_pt)
      (setq insPt (vlax-get obj 'InsertionPoint))

      (vla-Move
        obj
        (vlax-3d-point insPt)
        (vlax-3d-point '(0 0 0))
      )

      (vla-put-XScaleFactor obj 1.0)
      (vla-put-YScaleFactor obj 1.0)
      (vla-put-ZScaleFactor obj 1.0)
      
      (princ "\nBlock reference insertion point updated to (0,0).")
    )
    (princ "\nNo block reference found in the drawing.")
  )
  (princ)
)

;; Moves all objects in Paper Space to Model Space, except the Block Reference template
(defun c:MoveObjectsToModel (/ ss ent vpSS)
  (command "_.PSPACE")
  ; Select all objects in Paper Space
  ; DXF 67 = workspace (0 = MODEL, 1 = PAPER)
  (if (setq ss (ssget "_X" '((67 . 1))))
    ; Change space from Paper to Model
    (progn
      ; remove the title block from the selection set
      (if (setq ent (GetTitleBlock))
        (ssdel ent ss)
      )
      ; remove all viewports from the selection set
      (if (setq vpSS (ssget "_X" '((67 . 1) (0 . "VIEWPORT"))))
        (repeat (sslength vpSS)
          (ssdel (ssname vpSS 0) ss)
        )
      )

      (command "_.CHSPACE" ss "" "")
      (princ "\nObjects moved from Paper Space to Model Space.")
    )
    (princ "\nNo objects found in Paper Space (or only Block References).")
  )
  (command "_.MSPACE")
  (princ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TEXT FORMATTING FUNCTIONS
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
  (c:DeleteWipeouts)
  (c:MaskAllMText)
  (c:MaskAllMLeaders)
  (princ)
)

;; Applies a background color mask to all MTEXT objects
(defun c:MaskAllMText (/ ss i dxf_ent)
  (setq ss (ssget "_X" '((0 . "MTEXT"))))

  (cond
    (ss
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

      (princ "\nBackground mask set to drawing background color on MTEXT objects.")
    )
    ((princ "\nNo MText objects found."))
  )
  (princ)
)

;; Applies a background color mask to all MLEADER objects
(defun c:MaskAllMLeaders (/ ss i ent obj)


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

      (princ (strcat "\nEnabled background masks on " (itoa (sslength ss)) " MLEADER objects." ))
    )
    (princ "\nNo MLEADER objects found.")
  )

  (princ)
)

;; deletes all WIPEOUT objects
(defun c:DeleteWipeouts (/ ss)
  (if (setq ss (ssget "_X" '((0 . "WIPEOUT"))))
    (command "_.ERASE" ss "")
  )
  (princ)
)

;; Applies a background color mask to all MTEXT objects
(defun c:FormatMText (/ ss i ent obj)
  ; select all MTEXT objects 
  (if (setq ss (ssget "_X" '((0 . "MTEXT"))))
    (progn
      (setq i 0)
      ; repeat "length of ss (selection set)" times
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))
        
        ; Change any "STANDARD" font styles to "ROMANS"
        (if (wcmatch (strcase (vla-get-StyleName obj)) "STANDARD")
          (vla-put-StyleName obj "ROMANS")
        )

        ; Set font size based on font style
        ; If font style is Romans, set to 24; otherwise set to 48
        (if (wcmatch (strcase (vla-get-StyleName obj)) "ROMANS")
          (vla-put-Height obj 24)
          (vla-put-Height obj 48)
        )
        (setq i (1+ i))
      )
      (princ (strcat "\nFormatted " (itoa (sslength ss)) " MTEXT objects."))
    )
    (princ "\nNo MTEXT objects found.")
  )

  (princ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MISCELLANEOUS FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For some reason AutoCAD creates a new vertical MTEXT object in the bottom right with the .dwg's filepath.
;; I have no idea why this happens, but this function removes it if it exists
(defun c:DeletePathMText ( / ss i ent obj txt )
  (if (setq ss (ssget "_X" '((0 . "MTEXT"))))
    (repeat (setq i (sslength ss))
      (setq ent (ssname ss (setq i (1- i))))
      (setq obj (vlax-ename->vla-object ent))
      (setq txt (vla-get-TextString obj))

      (if (or (wcmatch txt "C:\\*") (wcmatch txt "\\W0.8500;C:\\*"))
        (entdel ent)
      )
    )
  )

  (princ)
)

;; Organizes draw order based on object properties:
;; 1. All XREFS to the back 
;; 2. All MTEXT objects to the front
;; 3. All geometry objects (circles, polylines, and lines) to the front
;; 4. Move VIEWPORT to the front so its above the title block
(defun c:OrderObjects (/ ss)
  ; select all external reference objects in the drawing
  (if (setq ss (ssget "_X" '((0 . "XREF"))))
    (progn
      ; send all external references to the back of the draw order
      (command "_.DRAWORDER" ss "" "B")
      (princ "\nAll external references sent to back of draw order.")
    )
    (princ "\nNo external references found in the drawing.")
  )
  
  ; select geometry objects (polylines, and lines)
  (if (setq ss (ssget "_X" '((0 . "LWPOLYLINE,POLYLINE,LINE"))))
    (progn
      ; send all geometry objects to the front of the draw order
      (command "_.DRAWORDER" ss "" "F")
      (princ "\nAll geometry objects sent to front of draw order.")
    )
    (princ "\nNo geometry objects found in the drawing.")
  )
  
  ; select all MTEXT objects in the drawing
  (if (setq ss (ssget "_X" '((0 . "MTEXT"))))
    (progn
    ; send all MTEXT objects to the front of the draw order
      (command "_.DRAWORDER" ss "" "F")
      (princ "\nMTEXT objects sent to front of draw order.")
    )
    (princ "\nNo MTEXT objects found in the drawing.")
  )
  
  ; select all Circle objects
  (if (setq ss (ssget "_X" '((0 . "CIRCLE"))))
    (progn
      ; send all Circle objects to the front of the draw order
      (command "_.DRAWORDER" ss "" "F")
      (princ "\nAll geometry objects sent to front of draw order.")
    )
    (princ "\nNo geometry objects found in the drawing.")
  )

  (command "_.PSPACE")
  ; select the VIEWPORT object and move it to the front so its above the title block
  (if (setq ss (ssget "_X" '((0 . "VIEWPORT"))))
    (progn
      (vl-cmdf "_.DRAWORDER" ss "" "F")
      (princ "\nViewport sent to front of draw order.")
    )
    (princ "\nNo viewport found in the drawing.")
  )
  (command "_.MSPACE")
  (princ)
)