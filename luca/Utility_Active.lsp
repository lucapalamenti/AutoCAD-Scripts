;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PAGE SETUP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Configures the PageSetup page
(defun SetupPageLayout (/)
	;; Configure page setup
	(command
		"_.-PLOT"
		"_Y" ; Detailed plot configuration?
		"" ; Current layout
		"AutoCAD PDF (General Documentation).pc3" ; Output device name
		"ANSI_full_bleed_B_(17.00_x_11.00_Inches)" ; Paper size
		"_Inches" ; Paper units
		"_Landscape" ; Paper orientation
		"_No" ; Plot upsidedown?
		"_Window" ; Plot area
		"0,0" ; Lower left corner
		"17,11" ; Upper right corner
		"_Fit" ; Plot scale
		"_Center" ; Plot offset
		"_Yes" ; Plot with plot styles?
		"." ; Plot style table name
		"_No" ; Plot with lineweights?
		"_No" ; Scale lineweights with plot scale?
		"_No" ; Plot paperspace first
		"_No" ; Hide paperspace objects
		(strcat (getvar "DWGNAME") ".pdf") ; File name
		"_Yes" ; Save changes to page setup
		"_No" ; Proceed with plot?
	)

	(princ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MISC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Organizes draw order based on object properties:
;; - All XREFS to the back 
;; - All MTEXT objects to the front
;; - All geometry objects (circles, polylines, and lines) to the front
(defun OrderObjects (/ ss)
	; send all external references to the back of the draw order
	(if (setq ss (ssget "_X" '((0 . "XREF"))))
        (command "_.DRAWORDER" ss "" "B")
		(PrincIf "\nNo external references found in the drawing.")
	)
	
	; send all geometry objects to the front of the draw order
	(if (setq ss (ssget "_X" '((0 . "LWPOLYLINE,POLYLINE,LINE"))))
		(command "_.DRAWORDER" ss "" "F")
		(PrincIf "\nNo geometry objects found in the drawing.")
	)
	
	; send all MTEXT objects to the front of the draw order
	(if (setq ss (ssget "_X" '((0 . "MTEXT"))))
		(command "_.DRAWORDER" ss "" "F")
		(PrincIf "\nNo MTEXT objects found in the drawing.")
	)
	
	; send all Circle objects to the front of the draw order
	(if (setq ss (ssget "_X" '((0 . "CIRCLE"))))
		(command "_.DRAWORDER" ss "" "F")
		(PrincIf "\nNo geometry objects found in the drawing.")
	)

	(princ)
)

;; Moves all objects in Paper Space to Model Space, except the title block
(defun MoveObjectsToModel (/ ss ent vpSS)
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
		)
		(PrincIf "\nNo objects found in Paper Space (or only Block References).")
	)
	(command "_.MSPACE")
	(princ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TEXT OBJECT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Converts all TEXT objects to MTEXT
(defun c:Text2MText (/ ss)
    (setq ss (ssget "_X" '((0 . "TEXT"))))
    (sssetfirst nil ss)
    (if ss
        (progn
            (command "_.TXT2MTXT" ss "")
            (PrincIf "All TEXT objects converted to MTEXT.")
        )
        (PrincIf "No TEXT objects found in the drawing.")
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
        (PrincIf "No MText objects found.")
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
        (PrincIf "No MLEADER objects found.")
    )
    (princ)
)

;; Deletes all WIPEOUT objects
(defun c:DeleteWipeouts (/ ss)
    (if (setq ss (ssget "_X" '((0 . "WIPEOUT"))))
        (command "_.ERASE" ss "")
    )
    (princ)
)