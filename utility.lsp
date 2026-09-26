;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UTILITY / DEBUGGING FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; prints out DXF codes for a selected object to the command line
; Mainly useful for debugging and understanding how to manipulate object properties with DXF codes
(defun c:ShowDXF (/ e)
  (if (setq e (car (entsel "\nSelect object: ")))
    (foreach x (entget e)
      (print x)
    )
  )
  (princ)
)

; Replaces and copies all attributes from one block reference to a new one
; @param newBlockReferenceFilePath [String] - File path to a .dwg file that contains a block reference
(defun ReplaceBlockReference ( oldEnt newBlockReferenceFilePath / newEnt )
  (command "_.PSPACE")
  
  (InsertBlock newBlockReferenceFilePath)
  (setq newEnt (entlast))
  (CopyBlockAttributes oldEnt newEnt)
  (entdel oldEnt)

  (command "_.MSPACE")
  (princ "Replaced Block Reference\n")
)

; INSERTs a block reference onto the drawing
(defun InsertBlock (filepath)
  (setvar "ATTREQ" 0)
  (command
    "_.-INSERT"
    filepath
    '(0 0 0) ; insertion point
    1 ; X scale
    1 ; Y scale
    0 ; rotation
  )
  (setvar "ATTREQ" 1)
  (princ)
)

; Copies attribute values from one block reference to another
; @param oldEnt [ENAME] - The entity name of the source block reference
; @param newEnt [ENAME] - The entity name of the target block reference
(defun CopyBlockAttributes (oldEnt newEnt / oldObj newObj oldMap val)
  (vl-load-com)

  (setq oldObj (vlax-ename->vla-object oldEnt)
        newObj (vlax-ename->vla-object newEnt))
  ;; Build a TAG -> VALUE map for old block
  (setq oldMap nil)

  (foreach att (vlax-invoke oldObj 'GetAttributes)
    (setq oldMap
      (cons
        (cons (strcase (vla-get-TagString att))
              (vla-get-TextString att))
        oldMap
      )
    )
  )
  
  ;; Apply values of map to matching tags in new block
  (foreach att (vlax-invoke newObj 'GetAttributes)
    (if (setq val
        (cdr
          (assoc
            (strcase (vla-get-TagString att))
            oldMap
          )
        )
      )
      (vla-put-TextString att val)
    )
  )

  (vla-Update newObj)
  (princ)
)

; Returns a list of file names from the provided file
; @param filename [string] - a file path
(defun FileToList (filename / fp result line)
  ; Initialize list of file paths
  (setq result '())

  (if (setq fp (open filename "r"))
    (progn
      (while (setq line (read-line fp))
        (setq result
          (cons
            (strcase (vl-string-trim " " line))
            result
          )
        )
      )
      (close fp)
    )
  )
  (princ (strcat "\nFound " (itoa (length result)) " names in " filename ":\n"))
  (reverse result)
)

; Pauses the script for a given time
; @params secs [Integer] - the number of seconds to pause the script for
(defun WaitSeconds (secs / endTime)
  (setq endTime (+ (getvar "DATE") (/ secs 86400.0)))
  (while (< (getvar "DATE") endTime))
)