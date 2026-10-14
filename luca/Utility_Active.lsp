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
		(princ "\nNo external references found in the drawing.")
	)
	
	; send all geometry objects to the front of the draw order
	(if (setq ss (ssget "_X" '((0 . "LWPOLYLINE,POLYLINE,LINE"))))
		(command "_.DRAWORDER" ss "" "F")
		(princ "\nNo geometry objects found in the drawing.")
	)

    ; send all LEADER and MLEADER objects to the front of the draw order
	(if (setq ss (ssget "_X" '((0 . "LEADER,MLEADER"))))
		(command "_.DRAWORDER" ss "" "F")
	)
	
	; send all MTEXT objects to the front of the draw order
	(if (setq ss (ssget "_X" '((0 . "MTEXT"))))
		(command "_.DRAWORDER" ss "" "F")
		(princ "\nNo MTEXT objects found in the drawing.")
	)
	
	; send all Circle objects to the front of the draw order
	(if (setq ss (ssget "_X" '((0 . "CIRCLE"))))
		(command "_.DRAWORDER" ss "" "F")
		(princ "\nNo geometry objects found in the drawing.")
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
		(princ "\nNo objects found in Paper Space (or only Block References).")
	)
	(command "_.MSPACE")
	(princ)
)


;; Replaces item at index n in lst
(defun ReplaceNth (n val lst)
    (if (zerop n)
        (cons val (cdr lst))
        (cons (car lst)
              (ReplaceNth (1- n) val (cdr lst)))
    )
)

;; Prompts the user to select LEADER objects and moves
;; the vertex at vIndex by dx,dy
;;
;; @param vIndex [INT] vertex index (0-based)
;; @param dx     [REAL] horizontal offset
;; @param dy     [REAL] vertical offset
(defun MoveLeaderVertex (vIndex dx dy / ss i obj coords idx vertexCount)
	(if (null vIndex) (setq vIndex (getint "Select Vertex Index:")))
	(if (null dx) (setq dx (getint "Set horizontal offset:")))
	(if (null dy) (setq dy (getint "Set vertical offset:")))
    (if (setq ss (ssget '((0 . "LEADER"))))
        (repeat (setq i (sslength ss))
            (setq obj (vlax-ename->vla-object (ssname ss (setq i (1- i)))) )
            (setq coords (vlax-safearray->list (vlax-variant-value (vla-get-Coordinates obj))))
            (setq vertexCount (/ (length coords) 3))
            (if (< vIndex vertexCount)
                (progn
                    (setq idx (* vIndex 3))
                    ;; Move X
                    (setq coords
                        (ReplaceNth
                            idx
                            (+ (nth idx coords) dx)
                            coords
                        )
                    )
                    ;; Move Y
                    (setq coords
                        (ReplaceNth
                            (+ idx 1)
                            (+ (nth (+ idx 1) coords) dy)
                            coords
                        )
                    )
                    ;; Update leader
                    (vla-put-Coordinates
                        obj
                        (vlax-make-variant
                            (vlax-safearray-fill
                                (vlax-make-safearray vlax-vbDouble (cons 0 (1- (length coords))))
                                coords
                            )
                        )
                    )
                    (vla-Update obj)
                )
            )
        )
    )

    (princ)
)
(defun c:MoveLeaderVertex (/)
	(MoveLeaderVertex nil nil nil)
	(princ)
)

(defun c:LeaderToMLeader (/ lead txt lobj tobj txtstr p1 p2)
    ; Prompt user to select LEADER
    (while (setq lead (car (entsel "\nSelect Leader <Enter to exit>: ")))
        (if (= "LEADER" (cdr (assoc 0 (entget lead))))
            (progn
                ; Prompt user to select MTEXT
                (setq txt (car (entsel "\nSelect MTEXT: ")))
                (if (= "MTEXT" (cdr (assoc 0 (entget txt))))
                    (progn
                        ; MTEXT contents
                        (setq txtstr (cdr (assoc 1 (entget txt))))
                        ;; LEADER arrow point
                        (setq p1 (cdr (assoc 10 (entget lead))))
                        ;; MTEXT insertion point
                        (setq p2 (GetLeaderVertex lead))

                        ;; create MLEADER
                        (command "_.MLEADER" p1 p2 txtstr)

                        ;; delete original objects
                        (entdel lead)
                        (entdel txt)

                        (princ "\nConverted.")
                    )
                    (princ "\nMTEXT not selected.")
                )
            )
            (princ "\nLeader not selected.")
        )
    )

    (princ)
)

(defun GetLeaderVertex (leader / coords)
    (setq coords
        (vlax-safearray->list
            (vlax-variant-value
                (vla-get-Coordinates
                    (vlax-ename->vla-object lead)
                )
            )
        )
    )
    (list (nth 3 coords) (nth 4 coords) (nth 5 coords))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TEXT OBJECT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Converts all TEXT objects to MTEXT
(defun Text2MText (/ ss)
    (setq ss (ssget "_X" '((0 . "TEXT"))))
    (sssetfirst nil ss)
    (if ss
        (command "_.TXT2MTXT" ss "")
    )
    (princ)
)

;; Runs all text masking functions
(defun c:MaskText (/)
    (Text2MText)
    (MaskAllMText)
    (MaskAllMLeaders)
    (DeleteWipeouts)
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
        (princ "No MText objects found.")
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
        (princ "No MLEADER objects found.")
    )
    (princ)
)

;; Deletes all WIPEOUT objects
(defun DeleteWipeouts (/ ss)
    (if (setq ss (ssget "_X" '((0 . "WIPEOUT"))))
        (command "_.ERASE" ss "")
    )
    (princ)
)