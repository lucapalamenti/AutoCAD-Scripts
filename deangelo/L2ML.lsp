(defun c:L2ML (/ lead txt lobj tobj txtstr p1 p2)

  (vl-load-com)

  (while
    (setq lead (car (entsel "\nSelect Leader <Enter to exit>: ")))

    (if (= "LEADER" (cdr (assoc 0 (entget lead))))
      (progn

        (setq txt (car (entsel "\nSelect MTEXT: ")))

        (if (= "MTEXT" (cdr (assoc 0 (entget txt))))
          (progn

            (setq txtstr (cdr (assoc 1 (entget txt))))

            ;; leader arrow point
            (setq p1 (cdr (assoc 10 (entget lead))))

            ;; mtext insertion point
            (setq p2 (cdr (assoc 10 (entget txt))))

            ;; create mleader
            (command
              "_.MLEADER"
              p1
              p2
              txtstr
            )

            ;; delete original objects
            (entdel lead)
            (entdel txt)

            (princ "\nConverted.")
          )
          (princ "\nMTEXT not selected.")
        )
      )
      (princ "\nLeader not selected.")
    )
  )

  (princ)
)