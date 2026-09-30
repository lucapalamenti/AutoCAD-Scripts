;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IMPORTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE SPECIFIC GLOBAL VARIABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Applies the given function "Work" to all .dwg files in the given folder. 
;; @param folder [string]
;; @param Work [function]
;; @param args [list]
(defun ApplyToAll (folder Work args / acad docs doc dwgPath layout ent)
	(princ "Start ApplyToAll function.\n")
	(setq acad (vlax-get-acad-object))
	(setq docs (vla-get-Documents acad))
	
	(foreach dwgPath (GetAllDwgs folder)
		; Open drawing
		(princ (strcat "Opening: " (vl-filename-base dwgPath) "\n"))
		(setq doc (vla-open docs dwgPath))
		
		; Call Work regardless of arguments
		
		; (apply Work args)
		
		; Save and close
		(princ (strcat "Closing: " (vl-filename-base dwgPath) "\n"))
		(vla-save doc)
		(vla-close doc)
	)
	(princ "End ApplyToAll function.\n")
)

(defun CountAllObjects (/ ss)
	(setq ss (ssget "_X"))
	
	(if ss
		(princ (strcat "\nTotal objects found: " (itoa (sslength ss))))
		(princ "\nNo objects found.")
	)
	
	(princ)
)

; Returns a list of all .dwg file paths as strings in a given directory
; @param folder [string] - a file directory
(defun GetAllDwgs (folder / result item fullpath)

	; Initialize list of DWG file paths
	(setq result '())

	; Add all DWG files in the current folder
	(foreach item (vl-directory-files folder "*.dwg" 1)
		(setq result (cons (strcat folder item) result))
	)

	; Recursively process all subfolders
	(foreach item (vl-directory-files folder nil -1)
		; Ignore the current and parent directory entries
		(if (and (/= item ".") (/= item ".."))
			(setq fullpath (strcat folder item)
				; Append DWGs found in this subfolder
				result (append result (GetAllDwgs fullpath))
			)
		)
	)
	(princ (strcat "\nFound " (itoa (length result)) " DWGs in directory " folder "\n"))
	result
)

(defun c:TestApplyToAll (/)
	(ApplyToAll
		*TEST_DIR*
		'testfunc1
		nil
	)
	(princ)
)