;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILEPATHS
;;; ---------
;;; ** UPDATE FILEPATHS TO BE THE FILEPATHS ON YOUR OWN LOCAL DEVICE
;;; ** MAKE SURE FILEPATHs HAVE DOUBLE BACKSLASHES \\
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(vl-load-com)

;;; FOLDERS

(setq *MAIN_DIR*
  "C:\\Users\\luca.palamenti\\OneDrive - RoviSys\\Documents\\AutoCAD\\")
(setq *TX_NUM*
  "TX309\\")
(setq *BMS_CP_DWGS*
  (strcat *MAIN_DIR* *TX_NUM* "BMS Control Panel Drawings DWGs\\"))
(setq *BMS_CP_PDFS*
  (strcat *MAIN_DIR* *TX_NUM* "BMS Control Panel Drawings PDFs\\"))
(setq *SCRIPT_DIR*
  (strcat *MAIN_DIR* "AutoCAD-Scripts\\"))

;;; INDIVIDUAL FILES

(setq *MONOCHROME_FILEPATH*
  (strcat *MAIN_DIR* "Grayscale_CP__Campus 11x17 model 2.dwg"))
(setq *BMS_SHOP_DWG_TITLEBLOCK_FILEPATH*
  (strcat *MAIN_DIR* *TX_NUM* "B Size RBT Final.dwg"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MAIN FUNCTION
;;; -------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Loads all scripts in the same directory is itself
;; Does NOT skip the Main.lsp file itself so that it can load itself into memory 
;; when updates are made
;; @param folder [string] filepath for a directory
(defun LoadAllScripts (folder / item fullPath)
  (foreach item (vl-directory-files folder nil 0)
    (setq fullPath (strcat folder item))
    (cond
      ;; Skip "." and ".."
      ((member item '("." "..")))

      ;; Load .lsp file
      ((wcmatch (strcase item) "*.LSP")
       (princ (strcat "\nLoading: " fullPath "\n"))
       (load fullPath)
      )

      ;; Recurse into subfolder
      ((vl-file-directory-p fullPath)
       (LoadAllScripts (strcat fullPath "\\"))
      )
    )
  )
  (princ)
)

(defun c:LoadAllScripts (/)
	(LoadAllScripts *SCRIPT_DIR*)
	(princ)
)
