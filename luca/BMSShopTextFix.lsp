(defun c:BMSShopTextFix (/ folder)
    (setq folder (getstring T "\nSelect folder for BMSShopTextFix: "))
    (ApplyToAll
        folder
        'BMSShopTextFix
        nil
    )
)
(defun BMSShopTextFix (doc /)
    (DeleteWipeouts doc)
    (princ)
)

;; Applies the given function "Work" to all .dwg files in the given folder. 
;; @param folder [STR]
;; @param Work [SYM]
;; @param args [LIST]
(defun ApplyToAll (folder Work args / docs doc dwgPath)
	(print "Start ApplyToAll -- ")
	(setq docs (vla-get-Documents (vlax-get-acad-object)))
	
	(foreach dwgPath (GetAllDwgs folder)
		; Open drawing
		(print (strcat "Opening: " (vl-filename-base dwgPath) "\n"))
		(setq doc (vla-open docs dwgPath))
		
		; Call Work regardless of arguments
		(apply Work (cons doc args))
		
		; Save and close
		(print (strcat "Closing: " (vl-filename-base dwgPath) "\n"))
		(vla-save doc)
		(vla-close doc)
	)
	(print "End ApplyToAll function.")
)
;; Returns a list of all .dwg file paths as strings in a given directory
;; @param folder [STR] - a file directory
;; @returns [LIST] a list of strings
(defun GetAllDwgs (folder / result item fullpath)
    (print "Start GetAllDwgs -- ")
	; Initialize list of DWG file paths
	(setq result '())

	; Add all DWG files in the current folder
	(foreach item (vl-directory-files folder "*.dwg" 1)
		(setq result (cons (strcat folder "\\" item) result))
	)

	; Recursively process all subfolders
	(foreach item (vl-directory-files folder nil -1)
		; Ignore the current and parent directory entries
		(if (and (/= item ".") (/= item ".."))
			(setq fullpath (strcat folder "\\" item)
				; Append DWGs found in this subfolder
				result (append result (GetAllDwgs fullpath))
			)
		)
	)
	(print (strcat "\nFound " (itoa (length result)) " DWGs in directory " folder "\n"))
	result
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TEXT OBJECT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Runs all text masking functions
(defun c:MaskText (/)
	(Text2MText)
    (MaskAllMText)
    (MaskAllMLeaders)
    (DeleteWipeouts)
    (princ)
)

;; Converts all TEXT objects to MTEXT
(defun Text2MText (/ ss)
    (setq ss (ssget "_X" '((0 . "TEXT"))))
    (sssetfirst nil ss)
    (if ss
        (command "_.TXT2MTXT" ss "")
    )
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
(defun DeleteWipeouts (/ ss)
    (if (setq ss (ssget "_X" '((0 . "WIPEOUT"))))
        (command "_.ERASE" ss "")
    )
    (princ)
)