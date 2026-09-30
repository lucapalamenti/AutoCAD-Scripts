(defun GetAllDwgs (folder / result item fullpath)

  (setq result '())

  (foreach item (vl-directory-files folder "*.dwg" 1)
    (setq result (cons (strcat folder "\\" item) result))
  )

  (foreach item (vl-directory-files folder nil -1)
    (if (and (/= item ".") (/= item ".."))
      (setq fullpath (strcat folder "\\" item)
            result (append result (GetAllDwgs fullpath))
      )
    )
  )

  result
)

(defun ReadTextFile (fname / f result line)

  (setq result '())

  (if (setq f (open fname "r"))
    (progn
      (while (setq line (read-line f))
        (setq result
              (cons
                (strcase (vl-string-trim " " line))
                result
              )
        )
      )
      (close f)
    )
  )

  (reverse result)
)

(defun DrawingInList (dwgPath drawingList / dwgName)

  (setq dwgName
        (strcase
          (vl-filename-base dwgPath)
        )
  )

  (member dwgName drawingList)
)

(defun c:UpdateRevisionBlock
       (/ folder listFile drawingList dwgs
          pos rev date year line1 line2 revBy apvBy
          revTag dateTag yearTag line1Tag line2Tag
          revByTag apvByTag drawingRevTag dateBlockTag
          acad docs doc file)

  (vl-load-com)

  (setq folder
        (getstring T "\nEnter top-level folder: ")
  )

  (setq listFile
        (getfiled
          "Select Drawing List"
          ""
          "txt"
          0
        )
  )

  (setq drawingList
        (ReadTextFile listFile)
  )

  (setq pos
        (getstring T "\nRevision Position Number: ")
  )

  (setq rev
        (getstring T "\nRevision Value: ")
  )

  (setq date
        (getstring T "\nRevision Date: ")
  )

  (setq year
        (getstring T "\nRevision Year: ")
  )

  (setq line1
        (getstring T "\nChanges Line 1: ")
  )

  (setq line2
        (getstring T "\nChanges Line 2: ")
  )

  (setq revBy
        (getstring T "\nRevision By: ")
  )

  (setq apvBy
        (getstring T "\nApproved By: ")
  )

  (setq revTag
        (strcase (strcat "POS_" pos "_REV"))
  )

  (setq dateTag
        (strcase (strcat "POS_" pos "_DATE"))
  )

  (setq yearTag
        (strcase (strcat "POS_" pos "_YEAR"))
  )

  (setq line1Tag
        (strcase (strcat "POS_" pos "_CHANGES_LINE_1"))
  )

  (setq line2Tag
        (strcase (strcat "POS_" pos "_CHANGES_LINE_2"))
  )

  (setq revByTag
        (strcase (strcat "POS_" pos "_REV_BY"))
  )

  (setq apvByTag
        (strcase (strcat "POS_" pos "_APV_BY"))
  )

  (setq drawingRevTag
        "DRAWING_REVISION"
  )

  (setq dateBlockTag
        "DATE"
  )

  (setq dwgs (GetAllDwgs folder))

  (princ
    (strcat
      "\nFound "
      (itoa (length dwgs))
      " drawings."
    )
  )

  (setq acad (vlax-get-acad-object))
  (setq docs (vla-get-Documents acad))

  (foreach file dwgs

    (if (DrawingInList file drawingList)

      (progn

        (princ
          (strcat
            "\nUpdating: "
            file
          )
        )

        (setq doc (vla-open docs file))

        (vlax-for layout (vla-get-Layouts doc)

          (setq block (vla-get-Block layout))

          (vlax-for ent block

            (if (and
                  (= (vla-get-ObjectName ent) "AcDbBlockReference")
                  (= :vlax-true (vla-get-HasAttributes ent))
                )

              (foreach att (vlax-invoke ent 'GetAttributes)

                (cond

                  ((= (strcase (vla-get-TagString att)) revTag)
                   (vla-put-TextString att rev))

                  ((= (strcase (vla-get-TagString att)) dateTag)
                   (vla-put-TextString att date))

                  ((= (strcase (vla-get-TagString att)) yearTag)
                   (vla-put-TextString att year))

                  ((= (strcase (vla-get-TagString att)) line1Tag)
                   (vla-put-TextString att line1))

                  ((= (strcase (vla-get-TagString att)) line2Tag)
                   (vla-put-TextString att line2))

                  ((= (strcase (vla-get-TagString att)) revByTag)
                   (vla-put-TextString att revBy))

                  ((= (strcase (vla-get-TagString att)) apvByTag)
                   (vla-put-TextString att apvBy))

                  ((= (strcase (vla-get-TagString att)) drawingRevTag)
                   (progn
                     (vla-put-TextString att rev)
                     (vla-put-ScaleFactor att 0.484)
                   ))

                  ((= (strcase (vla-get-TagString att)) dateBlockTag)
                   (vla-put-TextString att (strcat date "-" year)))

                )

              )
            )
          )
        )

        (vla-save doc)
        (vla-close doc)

      )
    )
  )

  (princ "\nDone.")
  (princ)
)