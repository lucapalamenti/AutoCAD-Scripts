(vl-load-com)

(defun UpdateRevisionDates ( / tb obj atts att tag val idx found )

  ;; Get title block ENAME using your existing function
  (setq tb (GetTitleBlock))

  (if tb
    (progn
      (setq obj  (vlax-ename->vla-object tb))
      (setq atts (vlax-invoke obj 'GetAttributes))

      ;; Look for POS_#_REV = 0.1
      (setq idx   1)
      (setq found nil)

      (while (and (<= idx 6) (not found))

        (foreach att atts
          (setq tag (strcase (vla-get-TagString att)))
          (setq val (vla-get-TextString att))

          (if (and
                (= tag (strcat "POS_" (itoa idx) "_REV"))
                (= val "0.1")
              )
            (setq found idx)
          )
        )

        (setq idx (1+ idx))
      )

      ;; If a matching revision was found
      (if found
        (progn

          ;; Update matching POS_#_DATE
          (foreach att atts
            (setq tag (strcase (vla-get-TagString att)))

            (cond
              ((= tag (strcat "POS_" (itoa found) "_DATE"))
               (vla-put-TextString att "09-25")
              )

              ((= tag "DATE")
               (vla-put-TextString att "09-25")
              )
            )
          )

          T
        )
        nil
      )
    )
  )
)

(defun GetFolderFromUser ( / file )
  (setq file (getfiled "Select any DWG in the desired folder" "" "dwg" 0 ) )
  (if file
    (vl-filename-directory file)
  )
)

(defun c:UpdateDrawingDates ( / folder dwgs acad docs doc fullpath )

  ;; Select folder
  (setq folder (GetFolderFromUser))

  (if folder
    (progn

      (setq acad (vlax-get-acad-object))
      (setq docs (vla-get-Documents acad))

      ;; All DWGs in folder
      (setq dwgs (vl-directory-files folder "*.dwg" 1))

      (foreach dwg dwgs

        (setq fullpath (strcat folder "\\" dwg))

        (princ (strcat "\nOpening: " fullpath))

        ;; Open drawing
        (setq doc (vla-open docs fullpath))

        ;; Make active
        (vla-activate doc)

        ;; Process title block
        ; (if (UpdateRevisionDates)
        ;   (progn
        ;     (princ "  -> Updated")
        ;     (vla-save doc)
        ;   )
        ;   (princ "  -> No matching revision")
        ; )

        ;; Close drawing
        (vla-close doc)
      )

      (princ "\nDone.")
    )
  )

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