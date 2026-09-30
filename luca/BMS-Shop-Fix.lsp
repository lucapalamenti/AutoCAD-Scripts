(defun c:BMSShopBorderFix (/)
    ; Configures the page layout
	(c:SetupLayout)
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