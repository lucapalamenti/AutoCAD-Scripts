;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OVERVIEW
;;; -------
;;; Contains functions for modifying the Title Block in .dwg files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IMPORTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(vl-load-com)
(load "AutoCAD-Scripts/utility.lsp")
(load "AutoCAD-Scripts/ApplyToAll.lsp")

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

;; Moves title block to PAPER space and formats it
;; Options: ShopBMS, PanelBMS
(defun c:FormatTitleBlock (/)
  (FormatTitleBlock_Helper nil)
  (princ)
)
(defun FormatTitleBlock_Helper (choice /)
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
      (FormatTitleBlock 1.0 1.0)
    )
    ((= choice "PanelBMS")
      (FormatTitleBlock 0.5 0.5)
    )
  )
)
(defun FormatTitleBlock (ScaleX ScaleY / ent ss obj insPt)
  (command "_.MSPACE")

  ; Move Title block to PAPER space and center it
  (setq ent (GetTitleBlock))
  (setq ss (ssadd ent (ssadd)))
  (if (> (sslength ss) 0)
    (progn
      (command "_.CHSPACE" ss "" "P")
      (setq obj (vlax-ename->vla-object ent))
      
      ; Set the block reference properties
      (setq insPt (vlax-get obj 'InsertionPoint))

      (vla-Move
        obj
        (vlax-3d-point insPt)
        (vlax-3d-point '(0 0 0))
      )

      (vla-put-XScaleFactor obj ScaleX)
      (vla-put-YScaleFactor obj ScaleY)
      (vla-put-ZScaleFactor obj 1.0)
      
      (princ "\n Title block insertion point updated to (0,0).")
    )
    (princ "\nNo block reference found in the drawing.")
  )
  (princ)
)

;; Replaces and copies all attributes from the current title block to a correctly formatted one
;; Options: ShopBMS
(defun c:ReplaceTitleBlock (/)
  (ReplaceTitleBlock_Helper nil)
  (princ)
)
(defun ReplaceTitleBlock (choice /)
  (if (null choice)
    (progn
      ;; Title Block options
      (initget "ShopBMS")
      ;; Prompt user
      (setq choice (getkword "\nSelect action [ShopBMS]: "))
    )
  )
  
  ;; Handle selection
  (cond
    ((= choice "ShopBMS")
      (ReplaceBlockReference (GetTitleBlock) *ShopDWGTitleBlockPath*)
    )
  )

  (princ (strcat "Replaced " choice " Title Block\n"))
  (princ)
)

;; Returns the ENAME of the Title Block entity
(defun GetTitleBlock (/ ss i ent obj blkName result)
  (setq ss (ssget "_X" '((0 . "INSERT"))))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i)
              obj (vlax-ename->vla-object ent)
              blkName (strcase (vla-get-EffectiveName obj)))
        ; If the Block Reference's name is the title block's name
        (if (member blkName *valid-title-block-names*)
          (progn
            (setq result ent)
            (setq i (sslength ss)) ; exit loop
          )
          (setq i (1+ i))
        )
      )
    )
  )
  result
)

;; Wrapper function for commandline call
(defun c:UpdateTitleBlockAttributes (/)
  (UpdateTitleBlockAttributes)
  (princ)
)

(defun UpdateTitleBlockAttributes ( / blk obj atts tag val )

  ;; Get title block ENAME
  (setq blk (GetTitleBlock))

  (if blk
    (progn
      (setq obj (vlax-ename->vla-object blk))

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
      (vl-cmdf "_.REGEN")
    )
  )

  (princ)
)

(defun c:UpdateTitleBlockAttributesAll (/)
  (c:ApplyToAll
    "C:\\Users\\luca.palamenti\\OneDrive - RoviSys\\Documents\\AutoCAD\\TX302\\Updated DWGs"
    "C:\\Users\\luca.palamenti\\OneDrive - RoviSys\\Documents\\AutoCAD\\scripts\\files.txt"
    'UpdateTitleBlockAttributes
    nil
  )
  (princ)
)