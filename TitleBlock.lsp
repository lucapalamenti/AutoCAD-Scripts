;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OVERVIEW
;;; -------
;;; Contains functions for modifying the Title Block in .dwg files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IMPORTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(vl-load-com)
(load "scripts/utility.lsp")
(load "scripts/ApplyToAll.lsp")

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

; Replaces and copies all attributes from the current title block to a correctly formatted one
(defun c:ReplaceBMSShopTitleBlock (/)
  (ReplaceBlockReference (GetTitleBlock) *ShopDWGTitleBlockPath*)
  (princ "- Replaced Title Block\n")
  (princ)
)

; Returns the ENAME of the Title Block entity
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

; Wrapper function for commandline call
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