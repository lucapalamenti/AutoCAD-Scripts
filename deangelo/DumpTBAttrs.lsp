(defun c:DumpTBAttrs ( / e obj att tag )
  (vl-load-com)

  (setq e (car (entsel "\nSelect title block: ")))

  (if e
    (progn
      (setq obj (vlax-ename->vla-object e))

      (princ "\n")
      (princ "\n===== TITLE BLOCK SCALE FACTORS =====")

      (foreach att (vlax-invoke obj 'GetAttributes)

        (setq tag (strcase (vla-get-TagString att)))

        (if
          (or
            (= tag "DRAWING_REVISION")
            (= tag "PROJECT_NUMBER")
          )
          (progn
            (princ
              (strcat
                "\n"
                tag
                " = "
                (rtos (vla-get-ScaleFactor att) 2 6)
              )
            )
          )
        )
      )

      (princ "\n=====================================")
    )
    (princ "\nNothing selected.")
  )

  (princ)
)