;;; @author - Luca Palamenti

;; These lists of names are pretty arbitrary. They are just the names that I've seen
;; floating around or used at some point
(setq *VALID-PANEL-TITLE-BLOCK-NAMES*
    '(
        "NEW_VDC_BLOCK-1"
		"BMS_PNL_TITLEBLOCK"
		"BMS_PANEL_TITLEBLOCK"
        "BMS_PNL_TB"
    )
)
(setq *VALID-SHOP-TITLE-BLOCK-NAMES*
    '(
        "BMS_SHOP_TITLEBLOCK"
        "BMS SHOP TITLEBLOCK"
		"B SIZE RBT"
		"B SIZE RBT FINAL"
		"B SIZE RBT CORRECTED"
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MAIN FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun c:BMSPanelStartScript ()
	(StartScript *VALID-PANEL-TITLE-BLOCK-NAMES* "BMS_Panel_TitleBlock.dwg" 0.5 0.5)
    (princ)
)
(defun c:BMSShopStartScript ()
	(StartScript *VALID-SHOP-TITLE-BLOCK-NAMES* "B Size RBT Final.dwg" 1 1)
	(princ)
)
;; NA works the same as BMS Shop because they use the same title block
(defun c:NAStartScript ()
	(StartScript *VALID-SHOP-TITLE-BLOCK-NAMES* "B Size RBT Final.dwg" 1 1)
	(princ)
)
(defun StartScript (nameList tbFilepath ScaleX ScaleY /)
    (setvar "CMDECHO" 0)

	; Go to first layout tab after Model tab
	(SetLayoutTab 1)
	(SetupPageLayout)
	(FixViewport)

    (ReplaceTitleBlock nameList tbFilepath ScaleX ScaleY)

    (setvar "CMDECHO" 1)
	;; Save the drawing and exit the console
	(command "._QSAVE")
	(princ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TITLE BLOCK FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ReplaceTitleBlock (nameList tbFilepath ScaleX ScaleY / oldtb newtb oldAtts ent data tag val pair att)
    (setq oldtb (GetTitleBlock nameList))
    (setq newtb (InsertTitleBlock tbFilepath ScaleX ScaleY))
    (if (and oldtb newtb)
        (progn
            ;; Collect attribute values from old title block
            (setq oldAtts '())
            (setq ent (entnext oldtb))
            (while ent
			
                (setq data (entget ent))
                (cond
                    ((null data)
                        (setq ent nil)
                    )
                    ((= "SEQEND" (cdr (assoc 0 data)))
                        (setq ent nil)
                    )
                    ((= "ATTRIB" (cdr (assoc 0 data)))
                        (setq oldAtts
                            (cons
                                (cons
                                    (strcase (cdr (assoc 2 data)))
                                    (cdr (assoc 1 data))
                                )
                                oldAtts
                            )
                        )
                        (setq ent (entnext ent))
                    )
                    (T
                        (setq ent (entnext ent))
                    )
                )
            )

            ;; Copy values into new title block
            (setq ent (entnext newtb))
            (while ent
                (setq data (entget ent))
                (cond
                    ((null data)
                        (setq ent nil)
                    )
                    ((= "SEQEND" (cdr (assoc 0 data)))
                        (setq ent nil)
                    )
                    ((= "ATTRIB" (cdr (assoc 0 data)))
                        (setq tag (strcase (cdr (assoc 2 data))))
                        (setq pair (assoc tag oldAtts))
                        (if pair
                            (progn
                                (setq val (cdr pair))
                                (setq att data)
                                (setq att
                                    (subst
                                        (cons 1 val)
                                        (assoc 1 att)
                                        att
                                    )
                                )
                                (entmod att)
                            )
                        )
                        (setq ent (entnext ent))
                    )
                    (T
                        (setq ent (entnext ent))
                    )
                )
            )
            ;; Delete old title block
            (entdel oldtb)
        )
		(print "One of the title blocks doesnt exist.")
    )
	(FormatTitleBlock newtb)
    newtb
)

;; Returns the entity name of the title block
;; @param validNames [LIST] list of strings that are valid title block names 
;; @returns [ENAME] title block entity, nil if not found
(defun GetTitleBlock (validNames / ss i ent blkname result)
    (setq ss (ssget "X" '((0 . "INSERT"))))
    (if ss
        (progn
            (setq i (sslength ss))
            (while (and (> i 0) (null result))
                (setq ent (ssname ss (setq i (1- i))))
                (setq blkname (strcase (cdr (assoc 2 (entget ent)))))

                (if (member blkname validNames)
                    (setq result ent)
                )
            )
        )
    )

    (if (null result) (print "No title block found."))
    result
)

;; Inserts a titleblock with the fiven filepathd
(defun InsertTitleBlock (filepath ScaleX ScaleY /)
	(command "_.PSPACE")

	;; Disable attribute prompts
	(setvar "ATTREQ" 0)
	(setvar "ATTDIA" 0)

    ;; Insert at 0,0 with scale 1,1 and rotation 0
    (command
        "_.-INSERT"
        filepath
        '(0.0 0.0 0.0)
        ScaleX
        ScaleY
        0.0
    )

	;; Enable attribute prompts
	(setvar "ATTDIA" 1)
	(setvar "ATTREQ" 1)

    (entlast)
)

;; Formats the title block on a drawing
(defun FormatTitleBlock (tb / tbData insPt)
    (setq tbData (entget tb))
    ;; Move title block to paper space if necessary
    (if (= "Model" (cdr (assoc 410 tbData)))
        (progn
            (setvar "CTAB" (car (layoutlist))) ; replace if layoutlist unavailable
            (command "_.CHSPACE" tb "")
            (setq tb (entlast))
            (setq tbData (entget tb))
        )
    )
    ;; Get insertion point
    (setq insPt (cdr (assoc 10 tbData)))
    ;; Move insertion point to 0,0,0
    (command
        "_.MOVE"
        tb
        ""
        insPt
        '(0.0 0.0 0.0)
    )
    (princ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UNIVERSAL FUNCTIONS (work for all dwg types)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Sets the layout tab to the given index
;; @param index [INT] tab index to select
(defun SetLayoutTab (index / dict layObj layData layName)
	(setq dict (dictsearch (namedobjdict) "ACAD_LAYOUT"))
	(while dict
		(cond
			((= 3 (caar dict))
				(setq layName (cdar dict))
			)
			((= 350 (caar dict))
				(setq layObj  (cdar dict))
				(setq layData (entget layObj))
				(if (= index (cdr (assoc 71 layData)))
					(setvar "CTAB" layName)
				)
			)
		
		)
		(setq dict (cdr dict))
	)
	(princ)
)

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

(defun FixViewport ( / ss1 viewport ss2 edata vp-id ptCenter curWidth curHeight targetWidth targetHeight deltaX deltaY ptBL ptTR ptBoxLL ptBoxUR oldWidth scaleFactor)
	(setq targetWidth 16.5)
	(setq targetHeight 10.5)
	(setq ss1 (ssget "X" (list '(0 . "VIEWPORT") (cons 410 (getvar "CTAB")))))
	
	(if ss1
		(progn
			;; Identify the viewport
			(setq viewport (ssname ss1 0))
			(setq edata (entget viewport))
			(setq vp-id (cdr (assoc 69 edata)))

			;; If the first viewport index is the Overall Layout Viewport (ID 1),
			;; look for the next viewport in the active layout selection set.
			(if (and (= vp-id 1) (> (sslength ss1) 1))
				(progn
					(setq viewport (ssname ss1 1))
					(setq edata (entget viewport))
					(setq vp-id (cdr (assoc 69 edata)))
				)
			)

			;; Safety check: Proceed only if we found a user-created viewport (ID > 1)
			(if (= vp-id 1)
				(princ "\nError: No user-created viewport found on this layout layout tab.")
				
				(progn
					;; Create selection set ss2 with just our target viewport object
					(setq ss2 (ssadd viewport (ssadd)))

					(setvar "OSMODE" 0) ; Turn off Osnap for exact coordinate math

					;; --- A. Calculate Current Dimensions ---
					(setq ptCenter (cdr (assoc 10 edata)))
					(setq curWidth  (cdr (assoc 40 edata)))
					(setq curHeight (cdr (assoc 41 edata)))
                    (setq oldWidth curWidth)

					;; Calculate distance needed to add/subtract to hit target sizes
					(setq deltaX (- targetWidth curWidth))
					(setq deltaY (- targetHeight curHeight))

					;; Find the precise physical coordinates of the Bottom-Left (BL) and Top-Right (TR) corners
					(setq ptBL (list (- (car ptCenter) (/ curWidth 2.0)) (- (cadr ptCenter) (/ curHeight 2.0)) 0.0))
					(setq ptTR (list (+ (car ptCenter) (/ curWidth 2.0)) (+ (cadr ptCenter) (/ curHeight 2.0)) 0.0))

					;; --- B. Stretch Width (Right Edge) ---
					;; Create a crossing window around the right two grips
					(setq ptBoxLL (list (car ptCenter) (- (cadr ptCenter) curHeight) 0.0))
					(setq ptBoxUR (list (+ (car ptTR) 5.0) (+ (cadr ptTR) 5.0) 0.0))
					
					; (command "_.stretch" "_C" ptBoxLL ptBoxUR "" '(0.0 0.0 0.0) (list deltaX 0.0 0.0))

					;; --- C. Stretch Height (Top Edge) ---
					;; Create a crossing window around the top two grips
					(setq ptBoxLL (list (- (car ptBL) 5.0) (cadr ptCenter) 0.0))
					(setq ptBoxUR (list (+ (car ptTR) (+ deltaX 5.0)) (+ (cadr ptTR) 5.0) 0.0))
					
					; (command "_.stretch" "_C" ptBoxLL ptBoxUR "" '(0.0 0.0 0.0) (list 0.0 deltaY 0.0))

                    ;; --- Re-read viewport after resize
                    (setq edata (entget viewport))
                    (setq curWidth (cdr (assoc 40 edata)))

                    ;; Calculate zoom factor so model scales with viewport size
                    (setq scaleFactor (/ curWidth oldWidth))

                    ; ;; Activate viewport and zoom model
                    ; (command "_.MSPACE")
                    ; (command "_.ZOOM" (strcat (rtos scaleFactor 2 8) "X"))
                    ; (command "_.PSPACE")

					;; --- D. Move Center to (8.5, 5.5) ---
					;; Re-read the entity because stretching changed its center location
					(setq edata (entget viewport))
					(setq ptCenter (cdr (assoc 10 edata)))
					
					(command "_.move" ss2 "" ptCenter '(8.5 5.5 0.0))

					;; --- E. Lock Viewport ---
					(command "_.mview" "_lock" "_on" ss2 "")
				)
			)
		)
		(princ "\nNo viewports found on this layout tab.")
	)
	(princ)
)