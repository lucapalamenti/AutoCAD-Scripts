;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IMPORTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(vl-load-com)
(load "AutoCAD-Scripts/luca/utility.lsp")
(load "AutoCAD-Scripts/luca/TitleBlock.lsp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GLOBAL VARIABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Border Fix for BMS Shop Drawings
(defun c:BMSShopBorderFix (/)
    ; Configures the page layout
	(c:SetupPageLayout)
	; Make sure the paper space is formatted correctly
	(c:FixViewport)
	(FormatTitleBlock_Helper "ShopBMS")
	; Move all objects to the Model workspace
	(c:MoveObjectsToModel)
	; Convert all TEXT objects to MTEXT objects
	(c:Text2MText)
	; Apply a BG mask to all text based objects
	(c:MaskText)
	; Apply certain text formatting to all MText objects
	(c:StyleMText)
	; Order objects so that some are in front of others
	(c:OrderObjects)
    (princ)
)

;; Border Fix for BMS Panel Drawings
(defun c:BMSPanelBorderFix (/)
	(ApplyToAll
	  *BMS_CP_DWGS*
	  'BMSPanelBorderFix
	  nil
	)
	(princ)
)
;; Border Fix for BMS Panel Drawings
;; @param doc [#<VLA-OBJECT IAcadDocument>] a document object
(defun BMSPanelBorderFix (doc /)
	(SetupPageLayout doc)
	(FixViewport doc)
	(FormatTitleBlock_Helper doc "PanelBMS")
	(c:MoveObjectsToModel)
	(princ)
)
