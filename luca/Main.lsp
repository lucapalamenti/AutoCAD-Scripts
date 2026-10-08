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
  	(strcat *MAIN_DIR* *TX_NUM* "BMS Control Panel Drawings DWGs"))
(setq *TEST_DIR*
  	(strcat *MAIN_DIR* *TX_NUM* "testdir\\"))
(setq *BMS_CP_PDFS*
  	(strcat *MAIN_DIR* *TX_NUM* "BMS Control Panel Drawings PDFs\\"))
(setq *SCRIPT_DIR*
  	(strcat *MAIN_DIR* "AutoCAD-Scripts\\luca\\"))

;;; INDIVIDUAL FILES

(setq *MONOCHROME_FILEPATH*
  	(strcat *MAIN_DIR* "Grayscale_CP__Campus 11x17 model 2.dwg"))
(setq *BMS_SHOP_DWG_TITLEBLOCK_FILEPATH*
  	(strcat *MAIN_DIR* "B Size RBT Corrected.dwg"))
(setq *BMS_PNL_DWG_TITLEBLOCK_FILEPATH*
  	(strcat *MAIN_DIR* "BMS_PNL_TB.dwg"))
(setq *TEST_LIST*
  	(strcat *MAIN_DIR* *TX_NUM* "testlist.txt"))

;;; OTHER

(setq *DEBUG_MODE* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MAIN FUNCTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Loads all scripts in the same directory is itself
;; Does NOT skip the Main.lsp file itself so that it can load itself into memory 
;; when updates are made
;; @param folder [STR] filepath for a directory
(defun LoadAllScripts (folder / item fullPath)
	; Preload certain files
	(load "AutoCAD-Scripts/luca/Utility_Standard.lsp")
	(foreach item (vl-directory-files folder nil 0)
		(setq fullPath (strcat folder item))
		(cond
			;; Skip "." and ".."
			((member item '("." "..")))

			;; Skip file "Main.lsp" (this file)
			; ((wcmatch (strcase item) "MAIN.LSP"))

			;; Skip file "Old.lsp"
			((wcmatch (strcase item) "OLD.LSP"))

			;; Skip file "Utility.lsp" (preloaded)
			((wcmatch (strcase item) "UTILITY_STANDARD.LSP"))

			;; Load .lsp file
			((wcmatch (strcase item) "*.LSP")
				(load fullPath)
			)

			;; Recurse into subfolder
			((vl-file-directory-p fullPath)
				(LoadAllScripts (strcat fullPath "\\"))
			)
		)
	)
	(print "Done loading scripts.")
	(princ)
)

(defun c:LoadAllScripts (/)
	(LoadAllScripts *SCRIPT_DIR*)
	(princ)
)