;; Runs the complete Shop Drawing Text Fix
;; Entire operation is grouped into one UNDO
(defun c:SHOPTXTFIX ()

    (command "_.UNDO" "_Begin")

    (c:DeleteWipeouts)
    (c:Text2MText)
    (MaskAllMText)
    (MaskAllMLeaders)

    (command "_.UNDO" "_End")

    (princ "\nSHOPTXTFIX complete.")
    (princ)
)

;; deletes all WIPEOUT objects
(defun c:DeleteWipeouts (/ ss)
    (if (setq ss (ssget "_X" '((0 . "WIPEOUT"))))
        (command "_.ERASE" ss "")
    )
    ;(princ)
)

;; Converts all TEXT objects to MTEXT
(defun c:Text2MText (/ ss)
    (setq ss (ssget "_X" '((0 . "TEXT"))))
    (sssetfirst nil ss)
    (if ss
        (progn
            (command "_.TXT2MTXT" ss "")
            (princ "\nAll TEXT objects converted to MTEXT.")
        )
        ;(princ "\nNo TEXT objects found in the drawing.")
    )
    ;(princ)
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
            (setq dxf_ent (append dxf_ent '((90 . 3) (63 . 256) (45 . 1.1) (41 . 0))))
            (entmod dxf_ent)
            (entupd (ssname ss i))
        )
        ;(princ "\nNo MText objects found.")
    )
    ;(princ)
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
        ;(princ "\nNo MLEADER objects found.")
    )
    ;(princ)
)
