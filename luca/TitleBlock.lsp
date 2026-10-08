;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OVERVIEW
;;; -------
;;; Contains functions for modifying the Title Block in .dwg files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IMPORTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(vl-load-com)
(load "AutoCAD-Scripts/luca/Utility_Standard.lsp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE SPECIFIC GLOBAL VARIABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq *valid-title-block-names*
	'(
		"BMS SHOP TITLEBLOCK"
		"B SIZE RBT"
		"B SIZE RBT FINAL"
		"B SIZE RBT CORRECTED"
		"NEW_VDC_BLOCK-1"
		"BMS_PNL_TB"
	))

; Used for updating the POS Block Reference attributes
; (POS 1 refers to the bottom row of the revisions table in the title block. POS 2 refers to the second to bottom row. etc.)


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
    (PrintIf "Finding title block -- ")

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
        (princ "No title block found.")
		(PrincIf "Title block found.")
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
	(PrintIf "Start FormatTitleBlock -- ")
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

			(TitleBlockAttCorrections doc tb choice)
		)
		(princ "No title block found in the drawing.")
	)
	(PrincIf "End FormatTitleBlock")
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
	(PrintIf "Start ReplaceTitleBlock -- ")
	(cond
		((null doc) (PrincIf "doc param not provided!"))
	)
	(if (null choice)
		(progn
			;; Title Block options
			(initget "ShopBMS PanelBMS")
			;; Prompt user
			(setq choice (getkword "\nSelect action [ShopBMS/PanelBMS]: "))
		)
	)

	(PrincIf "Replacing Title Block.")

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
	(PrincIf (strcat "\nReplaced " choice " Title Block."))
	tb
)

;; Makes sure the "AB_DWGNUMB" title block attribute is the same as the file name
;; @param doc [#<VLA-OBJECT IAcadDocument>] a document object
;; @param tb [#<VLA-OBJECT IAcadBlockReference] the title block object
(defun TitleBlockAttCorrections (doc tb choice / filename att)
	(PrintIf "Start MatchTitleBlockDWGNo -- ")
	(cond
		; Ensure all parameters are given
		((null doc) (PrincIf "doc param not provided!"))
		((null tb) (PrincIf "tb param not provided!"))
		((null choice) (PrincIf "choice param not provided!"))
		(T
			(setq filename (vl-filename-base (vla-get-Name doc)))
			(foreach att (vlax-invoke tb 'GetAttributes)
				(cond
					((= (strcase (vla-get-TagString att)) "AB_CADFILE")
						(vla-put-TextString att (cond
							((= choice "ShopBMS")
								"FilenameToCADPart_BMSShop FUNCTION NOT CREATED YET"
							)
							((= choice "PanelBMS")
								(FilenameToCADPart_BMSPNL filename)
							)
						))
					)
					((= (strcase (vla-get-TagString att)) "AB_DWGNUMB")
						(vla-put-TextString att filename)
					)
				)
				
			)
		)
	)
	(PrincIf "End MatchTitleBlockDWGNo.")
	(princ)
)
;; Converts a drawing filename to the AB_CADFILE attribute (CAD Part) for a title block
;; Examples:
;; DM11-BMS-PNL-02-C-01      -> BMS-PNL-DM-C-01
;; CP4-PUMP-BMS-PNL-01-M-06  -> BMS-PNL-PUMP-M-06
;; CP4-SUP-BMS-PNL-02-E-11   -> BMS-PNL-SUP-E-11
;; @param filename - [str]
(defun FilenameToCADPart_BMSPNL (filename / parts a c d f g)
	(setq parts (Str->List filename "-"))

	(cond
		; Format: AB-C-D-E-F-G ; Example: DM11-BMS-PNL-02-C-01
		((= (length parts) 6)
			(setq a (vl-string-right-trim "0123456789" (nth 0 parts)))
			(setq c (nth 1 parts))
			(setq d (nth 2 parts))
			(setq f (nth 4 parts))
			(setq g (nth 5 parts))
		)

		; Format: B-A-C-D-E-F-G ; Example: CP4-PUMP-BMS-PNL-01-M-06
		((= (length parts) 7)
			(setq a (nth 1 parts))
			(setq c (nth 2 parts))
			(setq d (nth 3 parts))
			(setq f (nth 5 parts))
			(setq g (nth 6 parts))
		)
	)

	(if (and a c d f g)
		(strcat c "-" d "-" a "-" f "-" g)
	)
)