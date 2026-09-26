;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IMPORTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(vl-load-com)

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
(defun c:ApplyToAll (folder Work args / allDwgs docs doc)
  (princ "Start ApplyToAll function.\n")
  (setq allDwgs (GetAllDwgs folder))

  ; Get AutoCAD document collection
  (setq acad (vlax-get-acad-object))
  (setq docs (vla-get-Documents acad))
  
  (foreach dwgPath allDwgs
    (progn
      (princ (strcat "IM HERE: " dwgPath "\n"))
      ; Open drawing
      (setq doc (vla-open docs dwgPath))
      (princ "1")
      ; Make it active
      (vla-activate doc)
      (princ "2")
      ; Call Work regardless of arguments
      (apply Work args)

      ; Save and close
      ; (vla-save (vla-get-ActiveDocument acad))
      (vla-close (vla-get-ActiveDocument (vlax-get-acad-object)))
    )
  )
  (princ "End ApplyToAll function.\n")
)

; Returns a list of all .dwg file paths as strings in a given directory
; @param folder [string] - a file directory
(defun GetAllDwgs (folder / result item fullpath)

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
  (princ (strcat "\nFound " (itoa (length result)) " DWGs in directory " folder "\n"))
  result
)

(defun c:TestApplyToAll (/)
  (c:ApplyToAll
    "C:\\Users\\luca.palamenti\\OneDrive - RoviSys\\Documents\\AutoCAD\\TX302\\test"
    'testfunc1
    nil
  )
  (princ)
)

(defun testfunc1 (/)
  (princ "I'm here.\n")
  (princ)
)