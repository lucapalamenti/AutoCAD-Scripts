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
(load "AutoCAD-Scripts/utility.lsp")
(load "AutoCAD-Scripts/TitleBlock.lsp")
(load "AutoCAD-Scripts/RevHelp.lsp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE SPECIFIC GLOBAL VARIABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MAIN FUNCTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Controls the flow of the script by calling all the other functions in the correct order
(defun c:BMSShopBorderFix (/)
	; Configures the page layout
	(c:SetupLayout)
	; Make sure the paper space is formatted correctly
	(c:FixViewport)
	(FormatTitleBlock_Helper "ShopBMS")
	; Move all objects to the Model workspace
	(c:MoveObjectsToModel)
	; ; Convert all TEXT objects to MTEXT objects
	; (c:Text2MText)
	; ; Apply a BG mask to all text based objects
	; (c:MaskText)
	; ; Apply certain text formatting to all MText objects
	; (c:StyleMText)
	; ; Order objects so that some are in front of others
	; (c:OrderObjects)

	(command "_.REGEN")

	(princ)
)

(defun c:BMSPanelBorderFix (/)
	(BMSPanelFix_Helper)
	; (ApplyToAll
	;   *TEST_DIR*
	;   'BMSPanelFix_Helper
	;   nil
	; )
	(princ)
)
(defun BMSPanelFix_Helper (/)
	(c:SetupLayout)
	(c:FixViewport)
	(FormatTitleBlock_Helper "PanelBMS")
	(c:MoveObjectsToModel)
	(princ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TITLE BLOCK AND SPACE FORMATTING FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; Applies text styling to all MTEXT objects
(defun c:StyleMText (/ ss i ent obj)
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

	(princ)
)