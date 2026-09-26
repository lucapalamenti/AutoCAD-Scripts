;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILEPATHS
;;; ---------
;;; ** UPDATE FILEPATHS TO BE THE FILEPATHS ON YOUR OWN LOCAL DEVICE
;;; ** MAKE SURE FILEPATHs HAVE DOUBLE BACKSLASHES \\
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
        (strcat *MAIN_DIR* "scripts\\"))

;;; INDIVIDUAL FILES

(setq *MONOCHROME_FILEPATH*
        (strcat *MAIN_DIR* "Grayscale_CP__Campus 11x17 model 2.dwg"))
(setq *BMS_SHOP_DWG_TITLEBLOCK_FILEPATH*
        (strcat *MAIN_DIR* *TX_NUM* "B Size RBT Final.dwg"))

(princ)