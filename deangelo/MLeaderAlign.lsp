(vl-load-com)

(defun ML:GetLineAndLeaderIndex (obj / lineIdx leaderIdx test i)

  (setq lineIdx nil
        i 0)

  (while (and (< i 100) (null lineIdx))

    (setq test
      (vl-catch-all-apply
        'vlax-invoke
        (list obj 'GetLeaderIndex i)
      )
    )

    (if (not (vl-catch-all-error-p test))
      (progn
        (setq lineIdx i)
        (setq leaderIdx test)
      )
    )

    (setq i (1+ i))
  )

  (if lineIdx
    (list lineIdx leaderIdx)
    nil
  )
)

(defun c:MALIGN
  (/ *error*
     acad doc undoStarted
     ss refEnt refObj
     refPair refLine refLeader
     refVerts refV1X
     refDir refDirX
     refDog

     i ent obj pair
     line leader
     dir dirX
     verts
     v0 v1 newVerts

     count skipped result
    )

  (setq acad (vlax-get-acad-object)
        doc  (vla-get-ActiveDocument acad)
        undoStarted nil)

  (defun *error* (msg)

    (if undoStarted
      (progn
        (vl-catch-all-apply
          'vla-EndUndoMark
          (list doc)
        )
        (setq undoStarted nil)
      )
    )

    (if
      (and
        msg
        (not
          (wcmatch
            (strcase msg)
            "*BREAK*,*CANCEL*,*EXIT*"
          )
        )
      )
      (princ
        (strcat "\nError: " msg)
      )
    )

    (princ)
  )

  (prompt "\nSelect MLeaders to align: ")

  (setq ss
    (ssget
      '((0 . "MULTILEADER"))
    )
  )

  (if ss

    (progn

      (setq refEnt
        (car
          (entsel
            "\nSelect GOOD reference MLeader: "
          )
        )
      )

      (if
        (and
          refEnt
          (= "MULTILEADER"
             (cdr (assoc 0 (entget refEnt)))
          )
        )

        (progn

          (setq refObj
            (vlax-ename->vla-object refEnt)
          )

          (setq refPair
            (ML:GetLineAndLeaderIndex refObj)
          )

          (if refPair

            (progn

              (setq refLine
                (car refPair)
              )

              (setq refLeader
                (cadr refPair)
              )

              ;; Reference vertices
              (setq refVerts
                (vlax-invoke
                  refObj
                  'GetLeaderLineVertices
                  refLine
                )
              )

              ;; V1 = elbow location
              (setq refV1X
                (nth 3 refVerts)
              )

              ;; Reference direction
              (setq refDir
                (vlax-invoke
                  refObj
                  'GetDoglegDirection
                  refLeader
                )
              )

              (setq refDirX
                (car refDir)
              )

              ;; Reference dogleg length
              (setq refDog
                (vla-get-DoglegLength refObj)
              )

              (princ
                (strcat
                  "\nReference Dogleg Length = "
                  (rtos refDog 2 4)
                )
              )

              (princ
                (strcat
                  "\nReference V1 X = "
                  (rtos refV1X 2 4)
                )
              )

              (vla-StartUndoMark doc)

              (setq undoStarted T
                    count 0
                    skipped 0
                    i 0)

              (repeat (sslength ss)

                (setq ent
                  (ssname ss i)
                )

                (setq obj
                  (vlax-ename->vla-object ent)
                )

                (setq pair
                  (ML:GetLineAndLeaderIndex obj)
                )

                (if pair

                  (progn

                    (setq line
                      (car pair)
                    )

                    (setq leader
                      (cadr pair)
                    )

                    (setq result
                      (vl-catch-all-apply
                        'vlax-invoke
                        (list
                          obj
                          'GetDoglegDirection
                          leader
                        )
                      )
                    )

                    (if (vl-catch-all-error-p result)

                      (progn

                        (setq skipped
                          (1+ skipped)
                        )

                        (princ
                          "\nSkipped one MLeader: could not read direction."
                        )

                      )

                      (progn

                        (setq dir result)
                        (setq dirX (car dir))

                        ;; only align leaders facing same way
                        (if

                          (or
                            (and (< refDirX 0.0)
                                 (< dirX 0.0))
                            (and (> refDirX 0.0)
                                 (> dirX 0.0))
                          )

                          (progn

                            (setq verts
                              (vlax-invoke
                                obj
                                'GetLeaderLineVertices
                                line
                              )
                            )

                            ;; V0 = arrowhead (unchanged)
                            (setq v0
                              (list
                                (nth 0 verts)
                                (nth 1 verts)
                                (nth 2 verts)
                              )
                            )

                            ;; V1 = elbow
                            (setq v1
                              (list
                                refV1X
                                (nth 4 verts)
                                (nth 5 verts)
                              )
                            )

                            (setq newVerts
                              (append v0 v1)
                            )

                            ;; Move elbow
                            (vlax-invoke
                              obj
                              'SetLeaderLineVertices
                              line
                              newVerts
                            )

                            ;; Match dogleg length
                            (vla-put-DoglegLength
                              obj
                              refDog
                            )

                            (vla-Update obj)

                            (setq count
                              (1+ count)
                            )

                          )

                          (progn

                            (setq skipped
                              (1+ skipped)
                            )

                            (princ
                              "\nSkipped opposite-facing MLeader."
                            )

                          )
                        )
                      )
                    )
                  )

                  (progn

                    (setq skipped
                      (1+ skipped)
                    )

                    (princ
                      "\nSkipped one MLeader: no valid leader index."
                    )

                  )
                )

                (setq i
                  (1+ i)
                )
              )

              (vla-EndUndoMark doc)

              (setq undoStarted nil)

              (princ
                (strcat
                  "\n\nAligned "
                  (itoa count)
                  " MLeader"
                  (if (= count 1) "" "s")
                  "."
                )
              )

              (if (> skipped 0)

                (princ
                  (strcat
                    "\nSkipped "
                    (itoa skipped)
                    " MLeader"
                    (if (= skipped 1) "" "s")
                    "."
                  )
                )
              )

            )

            (prompt
              "\nReference MLeader has no valid leader."
            )
          )
        )

        (prompt
          "\nReference object must be a Multileader."
        )
      )
    )

    (prompt
      "\nNo MLeaders selected."
    )
  )

  (princ)
)

(princ "\nMALIGN loaded.")
(princ)