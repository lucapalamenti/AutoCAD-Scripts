(defun c:BMSShopBorderFix (/)
    ; Configures the page layout
    (c:SetupLayout)
    ; Make sure the paper space is formatted correctly
    (c:FixViewport)
    (FormatTitleBlock_Helper "ShopBMS")
    ; Move all objects to the Model workspace
    (c:MoveObjectsToModel)
    (princ)
)