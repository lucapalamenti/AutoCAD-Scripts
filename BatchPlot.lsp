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

(defun c:CreateDSDs_TX309-BMS-PNL (/)
  (CreateDSDs *BMS_CP_DWGS* T)
  (princ)
)

;;
(defun CreateDSDs (directory makeDsdCopyDir / dsdFilepaths subFolders folder dsdFilepath files file dsdfp count dsdCopyDir dsdCopyDirFilepath)
  ; List of .dsd filepaths to be returned
  (setq dsdFilepaths '())
  (setq subFolders (GetLeafFolders directory))
  ; Iterate over each subfolder
  (foreach folder subFolders
    (setq dsdFilepath (strcat folder "\\" (vl-filename-base folder) ".dsd"))
    ; Add current dsd to end of list
    (setq dsdFilepaths (cons dsdFilepath dsdFilepaths))
    ; .dsd file pointer
    (setq dsdfp (open dsdFilepath "w"))

    (write-line "[DWF6Version]" dsdfp)
    (write-line "Ver=1" dsdfp)
    (write-line "[DWF6MinorVersion]" dsdfp)
    (write-line "MinorVer=1" dsdfp)

    (setq count 0)
    (setq files (GetBMSPNLFiles folder))
    (foreach file files
      (setq count (1+ count))
      (write-line (strcat "[DWF6Sheet:" file "]") dsdfp)
      (write-line (strcat "DWG=" folder "\\" file) dsdfp)
      (write-line "Layout=Layout1" dsdfp)
      (write-line (strcat "Setup=Monochrome|" *MONOCHROME_FILEPATH*) dsdfp)
    )

    (write-line "[Target]" dsdfp)
    (write-line "Type=6" dsdfp)
    ; (write-line (strcat "DWF=" folder "\\" (vl-filename-base folder) ".pdf") dsdfp)
    (write-line (strcat "DWF=" *BMS_CP_PDFS* (vl-filename-base folder) ".pdf") dsdfp)
    (write-line (strcat "OUT=" *BMS_CP_PDFS*) dsdfp)
    (write-line "PWD=" dsdfp)

    (write-line "[PdfOptions]" dsdfp)
    (write-line "IncludeHyperlinks=TRUE" dsdfp)
    (write-line "CreateBookmarks=TRUE" dsdfp)
    (write-line "CaptureFontsInDrawing=TRUE" dsdfp)
    (write-line "ConvertTextToGeometry=FALSE" dsdfp)
    (write-line "VectorResolution=1200" dsdfp)
    (write-line "RasterResolution=400" dsdfp)

    (write-line "[PublishType]" dsdfp)
    (write-line "PublishType=1" dsdfp)
    (write-line "[SheetSet Properties]" dsdfp)
    (write-line "NoOfCopies=1" dsdfp)
    (write-line (strcat "NumberOfSheets=" (itoa count)) dsdfp)

    (close dsdfp)

    ; Handle making copy directory if requested
    (if makeDsdCopyDir
      (progn
        (setq dsdCopyDir (strcat *BMS_CP_DWGS* "sheetSets\\"))
        ; Make the directory if it doesnt exist
        (if (not (vl-file-directory-p dsdCopyDir))
          (vl-mkdir dsdCopyDir)
        )

        (setq dsdCopyDirFilepath (strcat dsdCopyDir (vl-filename-base folder) ".dsd"))
        ; If the file already exists, delete the old one
        (if (findfile dsdCopyDirFilepath)
          (vl-file-delete dsdCopyDirFilepath)
        )
        ; Make a copy of the file in a location that will contain all .dsd files for the given directory
        (vl-file-copy dsdFilepath dsdCopyDirFilepath)
      )
    )
  )
  (princ (strcat "DSD created for " (itoa (length subFolders)) " subfolders.\n"))
  ; Return list in original order
  (reverse dsdFilepaths)
)

;; Returns a list of all "leaf node" folders in a directory as filepaths.
;; *Any folder that does not contain any subfolders is considered a "leaf node"
;; @param directory [string] - a filepath
;; @returns [list[string]] - all "leaf node" folders in a directory as filepaths
(defun GetLeafFolders (directory / subFolders result)
  (setq subFolders
    (mapcar
      '(lambda (x) (strcat (vl-string-right-trim "\\" directory) "\\" x))
      (vl-remove-if
        '(lambda (x) (member x '("." "..")))
        (vl-directory-files directory nil -1)
      )
    )
  )

  (if subFolders
    ; Has children -> recurse into each child
    (apply 'append (mapcar 'GetLeafFolders subFolders))
    ; No children -> this is a leaf folder
    (list directory)
  )
)

;; Returns a list of BMS PNL dwg file names in the correct order (C, M, E, IO, I)
;; @param directory [string] - a directory's filepath
;; @returns [list[string]] - a list of file names as strings
(defun GetBMSPNLFiles (directory / Cfiles Mfiles Efiles IOfiles Ifiles files)
  (setq Cfiles (vl-directory-files directory "*-C-*.dwg" 1))
  (setq Mfiles (vl-directory-files directory "*-M-*.dwg" 1))
  (setq Efiles (vl-directory-files directory "*-E-*.dwg" 1))
  (setq IOfiles (vl-directory-files directory "*-IO-*.dwg" 1))
  (setq Ifiles (vl-directory-files directory "*-I-*.dwg" 1))
  (append Cfiles Mfiles Efiles IOfiles Ifiles)
)
