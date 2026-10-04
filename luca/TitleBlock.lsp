;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OVERVIEW
;;; -------
;;; Contains functions for modifying the Title Block in .dwg files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IMPORTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(vl-load-com)
(load "AutoCAD-Scripts/luca/utility.lsp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE SPECIFIC GLOBAL VARIABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq *valid-title-block-names*
	'(
		"B SIZE RBT"
		"B SIZE RBT FINAL"
		"B SIZE RBT CORRECTED"
		"NEW_VDC_BLOCK-1"
	))

; Used for updating the POS Block Reference attributes
; (POS 1 refers to the bottom row of the revisions table in the title block. POS 2 refers to the second to bottom row. etc.)
(setq *POS* "3")
; Update to contain new attribute values
(setq *attributeMap*
	(list
		(cons                      "DRAWING_REVISION" "0.1")
		(cons                      "DATE"             "09-25-2026")
		(cons (strcat "POS_" *POS* "_REV")            "0.1")
		(cons (strcat "POS_" *POS* "_DATE")           "09-25")
		(cons (strcat "POS_" *POS* "_YEAR")           "2026")
		(cons (strcat "POS_" *POS* "_CHANGES_LINE_1") "IFC REV 0 COMMENTS")
		(cons (strcat "POS_" *POS* "_CHANGES_LINE_2") "")
		(cons (strcat "POS_" *POS* "_REV_BY")         "LP")
		(cons (strcat "POS_" *POS* "_APV_BY")         "SA")
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Returns the Title Block of the active document as a VLA-OBJECT
;; @returns [#<VLA-OBJECT IAcadBlockReference]
(defun c:GetTitleBlock (/)
    (GetTitleBlock (GetActiveDoc))
    (princ)
)
;; Returns the Title Block of a given document as a VLA-OBJECT
;; @param doc [#<VLA-OBJECT IAcadDocument>] a document object
(defun GetTitleBlock (doc / obj result)
    (princ "Finding title block.\n")

    (if (null doc)
        (setq doc (GetActiveDoc))
    )

	; Search paperspace for title block
    (vlax-for obj (vla-get-PaperSpace doc)
        (if (= "AcDbBlockReference" (vla-get-ObjectName obj))
			(if (member (strcase (vla-get-EffectiveName obj)) *valid-title-block-names*)
				(setq result obj)
			)
        )
    )
	; Search modelspace for title block
    (vlax-for obj (vla-get-ModelSpace doc)
        (if (= "AcDbBlockReference" (vla-get-ObjectName obj))
			(if (member (strcase (vla-get-EffectiveName obj)) *valid-title-block-names*)
				(setq result obj)
			)
        )
    )

    (if (null result)
        (princ "\nNo title block found.")
		(princ "\nTitle block found.")
    )
    result
)

;; Moves title block to PAPER space and formats it
;; Options: ShopBMS, PanelBMS
(defun c:FormatTitleBlock (/)
	(FormatTitleBlock_Prompt (GetActiveDoc) nil)
	(princ)
)
;; Prompt user for title block type
;; @param doc [#<VLA-OBJECT IAcadDocument>] a document object
;; @param choice [STR] ShopBMS or PanelBMS
(defun FormatTitleBlock_Prompt (doc choice /)
	; Make sure doc is defined
    (if (null doc)
        (setq doc (GetActiveDoc))
    )
	; Make sure choice is defined
	(if (null choice)
		(progn
			;; Title Block options
			(initget "ShopBMS PanelBMS")
			;; Prompt user
			(setq choice (getkword "\nSelect action [ShopBMS/PanelBMS]: "))
		)
	)
	
	;; Handle selection
	(cond
		((= choice "ShopBMS")
			(FormatTitleBlock doc choice 1.0 1.0)
		)
		((= choice "PanelBMS")
			(FormatTitleBlock doc choice 0.5 0.5)
		)
	)
)
;; Formats the title block on a drawing
;; @param doc [#<VLA-OBJECT IAcadDocument>] a document object
;; @param choice [STR] ShopBMS or PanelBMS
;; @param ScaleX [REAL]
;; @param ScaleY [REAL]
(defun FormatTitleBlock (doc choice ScaleX ScaleY / insPt tb)
	(princ "\nStart: FormatTitleBlock")
	; Move Title block to PAPER space and center it
	(setq tb (GetTitleBlock doc))
	(if tb
		(progn
			; Make sure the title block is in PAPER space
			(setq tb (ReplaceTitleBlock doc choice))

			(setq insPt (vlax-get tb 'InsertionPoint))
			(vla-Move
				tb
				(vlax-3d-point insPt)
				(vlax-3d-point '(0 0 0))
			)

			(vla-put-XScaleFactor tb ScaleX)
			(vla-put-YScaleFactor tb ScaleY)
			(vla-put-ZScaleFactor tb 1.0)
			
			(princ "\n Title block insertion point updated to (0,0).")
		)
		(princ "\nNo title block found in the drawing.")
	)
	(princ "\nEnd: FormatTitleBlock")
	(princ)
)

;; Replaces and copies all attributes from the current title block to a correctly formatted one
;; in paperspace
;; Options: ShopBMS
(defun c:ReplaceTitleBlock (/)
	(ReplaceTitleBlock (GetActiveDoc) nil)
	(princ)
)
;; Replaces the title block on a drawing
;; @param doc [#<VLA-OBJECT IAcadDocument>] a document object
;; @param choice [STR] ShopBMS or PanelBMS
(defun ReplaceTitleBlock (doc choice / tb)
	(if (null doc)
		(setq doc (GetActiveDoc))
	)
	(if (null choice)
		(progn
			;; Title Block options
			(initget "ShopBMS PanelBMS")
			;; Prompt user
			(setq choice (getkword "\nSelect action [ShopBMS/PanelBMS]: "))
		)
	)

	(princ "\nReplacing Title Block.")

	(setq tb (GetTitleBlock doc))
	
	;; Handle selection
	(cond
		((= choice "ShopBMS")
			(setq tb (ReplaceBlockReference doc 'PAPER tb *BMS_SHOP_DWG_TITLEBLOCK_FILEPATH*))
		)
		((= choice "PanelBMS")
			(setq tb (ReplaceBlockReference doc 'PAPER tb *BMS_PNL_DWG_TITLEBLOCK_FILEPATH*))
		)
	)
	(princ (strcat "\nReplaced " choice " Title Block."))
	tb
)

(defun c:UpdateTitleBlockAttributes (/)
	(UpdateTitleBlockAttributes)
	(princ)
)

;; @param doc [#<VLA-OBJECT IAcadDocument>] a document object
(defun UpdateTitleBlockAttributes (doc / tb obj atts tag val )
	;; Get title block ENAME
	(setq tb (GetTitleBlock))

	(if tb
		(progn
			(setq obj (vlax-ename->vla-object tb))

			;; Iterate all attribute references
			(foreach att (vlax-invoke obj 'GetAttributes)

				(setq tag (strcase (vla-get-TagString att)))

				;; Lookup value in *attributeMap*
				(if (setq val (cdr (assoc tag *attributeMap*)))
					(progn
						(vla-put-TextString att val)

						;; Force alignment recalculation for FIT attributes
						(if (vlax-property-available-p att 'TextAlignmentPoint)
							(vla-put-TextAlignmentPoint
								att
								(vla-get-TextAlignmentPoint att)
							)
						)
						(vla-Update att)
					)
				)
			)
			(vla-Update obj)
		)
		(princ "\nTitle block not found.")
	)

	(princ)
)

(defun c:UpdateTitleBlockAttributesAll (/)
	(c:ApplyToAll
		*BMS_CP_DWGS*
		'UpdateTitleBlockAttributes
		nil
	)
	(princ)
)