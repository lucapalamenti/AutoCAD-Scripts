;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IMPORTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(vl-load-com)

(setq *valid-title-block-names*
    '(
        "NEW_VDC_BLOCK-1"
		"BMS_PNL_TITLEBLOCK"
		"BMS_PANEL_TITLEBLOCK"
        "BMS_PNL_TB"
        "BMS_SHOP_TITLEBLOCK"
        "BMS SHOP TITLEBLOCK"
		"B SIZE RBT"
		"B SIZE RBT FINAL"
		"B SIZE RBT CORRECTED"
    )
)
(setq *DEBUG_MODE* T)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MAIN FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun c:BMSPanelStartScript (/ tbfilepath)
    (setq tbfilepath "C:\\Users\\luca.palamenti\\OneDrive - RoviSys\\Documents\\AutoCAD\\BMS_PNL_TB.dwg")
    (if (null tbfilepath)
        (setq tbfilepath (getfiled "Select filepath for BMS Panel Title Block: " "" "dwg" 0))
    )
    (ApplyToAll
        nil
        'WorkScript
        (list
            tbfilepath
            0.5
            0.5
        )
    )
    (princ)
)
(defun c:BMSShopStartScript (/ tbfilepath)
    (setq tbfilepath nil)
    (if (null tbfilepath)
        (setq tbfilepath (getfiled "Select filepath for BMS Shop Title Block: " "" "dwg" 0))
    )
    (ApplyToAll
        nil  
        'WorkScript
        (list
            tbfilepath
            1
            1
        )
    )
	(princ)
)
;; NA works the same as BMS Shop because they use the same title block
(defun c:NAStartScript (/ tbfilepath)
    (setq tbfilepath nil)
    (if (null tbfilepath)
        (setq tbfilepath (getfiled "Select filepath for NA Title Block: " "" "dwg" 0))
    )
    (ApplyToAll
        nil  
        'WorkScript
        (list
            tbfilepath
            1
            1
        )
    )
	(princ)
)

(defun WorkScript (doc tbFilepath ScaleX ScaleY /)
	(SetupPageLayout doc)
	(FixViewport doc)
    (ReplaceBlockReference doc 'PAPER (GetTitleBlock doc) tbFilepath ScaleX ScaleY)
	(princ)
)

;; Applies the given function "Work" to all .dwg files in the given folder. 
;; @param folder [STR]
;; @param Work [SYM]
;; @param args [LIST]
(defun ApplyToAll (folder Work args / docs doc dwgPath)
	(PrintIf "Start ApplyToAll -- ")
    ; Prompt user until a non empty string is provided
    (while (or (null folder) (= folder ""))
        (setq folder (getstring T "\nEnter drawings folder path: "))
    )
	(setq docs (vla-get-Documents (vlax-get-acad-object)))
	
	(foreach dwgPath (GetAllDwgs folder)
		; Open drawing
		(PrincIf (strcat "Opening: " (vl-filename-base dwgPath) "\n"))
		(setq doc (vla-open docs dwgPath))
		
		; Call Work regardless of arguments
		(apply Work (cons doc args))
		
		; Save and close
		(PrincIf (strcat "Closing: " (vl-filename-base dwgPath) "\n"))
		(vla-save doc)
		(vla-close doc)
	)
	(PrincIf "End ApplyToAll function.")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OBJECT GETTERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Returns the layout tab at the given index for a given document. "Model" is always index 0.
;; @param doc [#<VLA-OBJECT IAcadDocument>] a document object
;; @param index [INT] tab index
;; @returns [#<VLA-OBJECT IAcadLayout>]
(defun GetLayoutTab (doc index / lay layout)
    (vlax-for lay (vla-get-Layouts doc)
        (if (= (vla-get-TabOrder lay) index)
            (setq layout lay)
        )
    )
    layout
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PAGE SETUP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Configures the size and positioning of a page
;; @param doc [#<VLA-OBJECT IAcadDocument>] a document object
(defun SetupPageLayout (doc / lay)
    (PrintIf "Start SetupPageLayout -- ")

    (setq lay (GetLayoutTab doc 1))

    (vla-RefreshPlotDeviceInfo lay)
    ; Set Printer/plotter name
    (vla-put-ConfigName lay "AutoCAD PDF (General Documentation).pc3")
    ; Set Paper size
    (vla-put-CanonicalMediaName lay "ANSI_full_bleed_B_(17.00_x_11.00_Inches)")
    ; Set Drawing orientation to Landscape
    (vla-put-PlotRotation lay ac0degrees)
    ; Set plot window bounds to [(0,0), (17,11)]
    (vla-SetWindowToPlot lay
        ; Bottom Left coordinate
        (vlax-make-variant
            (vlax-safearray-fill
                ; Create a safearray of 2 doubles (indices 0 to 1 inclusive)
                (vlax-make-safearray vlax-vbDouble '(0 . 1)) '(0.0 0.0)
            )
        )
        ; Top Right coordinate
        (vlax-make-variant
            (vlax-safearray-fill
                ; Create a safearray of 2 doubles (indices 0 to 1 inclusive)
                (vlax-make-safearray vlax-vbDouble '(0 . 1)) '(17.0 11.0)
            )
        )
    )
    ; Set "What to plot:" to 4 (Window)
    (vla-put-PlotType lay 4)
    ; Set Plot Offset to X: 0, Y: 0
    (vla-put-PlotOrigin lay (vlax-make-variant
        (vlax-safearray-fill
            ; Create a safearray of 2 doubles (indices 0 to 1 inclusive)
            (vlax-make-safearray vlax-vbDouble '(0 . 1)) '(0.0 0.0)
        )
    ))
    (PrincIf "End SetupPageLayout")
    (princ)
)

;; Make sure the VIEWPORT is ordered, centered, & sized correctly
;; @param doc [#<VLA-OBJECT IAcadDocument>] a document object
(defun FixViewport (doc / viewport obj)
    (PrintIf "Start FixViewport -- ")
    ;; Find first viewport in paperspace
    (vlax-for obj (vla-get-PaperSpace doc)
        (if (= "AcDbViewport" (vla-get-ObjectName obj))
            (setq viewport obj)
        )
    )
    
    ; Move VIEWPORT to the front so its above the title block
    ; (vla-MoveToTop viewport) ; * doesnt exist in AutoCAD LT *

    ;; Lock viewport
    (vla-put-DisplayLocked viewport :vlax-true)

    ;; Set viewport properties
    (vla-put-Center viewport (vlax-3d-point 8.5 5.5 0))
    (vla-put-Height viewport 10.5)
    (vla-put-Width  viewport 16.5)

    (PrincIf "Viewport properties updated: Center (8.5,5.5), Height 10.5, Width 16.5, moved to front.")
    (princ)
)

;; Returns the Title Block of a given document as a VLA-OBJECT
;; @param doc [#<VLA-OBJECT IAcadDocument>] a document object
(defun GetTitleBlock (doc / obj result)
    (PrintIf "Finding title block -- ")

	; Search paperspace for title block
    (vlax-for obj (vla-get-PaperSpace doc)
        (if (= "AcDbBlockReference" (vla-get-ObjectName obj))
			(if (member (strcase (vla-get-EffectiveName obj)) *valid-title-block-names*)
				(setq result obj)
			)
        )
    )
	; Search modelspace for title block
    (vlax-for obj (vla-get-ModelSpace doc)
        (if (= "AcDbBlockReference" (vla-get-ObjectName obj))
			(if (member (strcase (vla-get-EffectiveName obj)) *valid-title-block-names*)
				(setq result obj)
			)
        )
    )

    (if (null result)
        (princ "No title block found.")
		(PrincIf "Title block found.")
    )
    result
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BLOCK REFERENCE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Replaces and copies all attributes from one block reference to a new one, returning the new object
;; @param doc [#<VLA-OBJECT IAcadDocument>] a document object
;; @param chooseSpace [SYM] MODEL or PAPER
;; @param oldObj [#<VLA-OBJECT IAcadBlockReference]
;; @param newObjFilePath [STR] File path to a .dwg file that contains a block reference
;; @returns [#<VLA-OBJECT IAcadBlockReference] the new block reference
(defun ReplaceBlockReference (doc chooseSpace oldObj newObjFilePath ScaleX ScaleY / newObj)
    (setq newObj (InsertBlockReference doc chooseSpace newObjFilePath))
    (setq insPt (vlax-get newObj 'InsertionPoint))
    (vla-Move
        newObj
        (vlax-3d-point insPt)
        (vlax-3d-point '(0 0 0))
    )
    (vla-put-XScaleFactor newObj ScaleX)
    (vla-put-YScaleFactor newObj ScaleY)
    (vla-put-ZScaleFactor newObj 1.0)
    (CopyBlockAttributes oldObj newObj)
    (vla-Delete oldObj)
    (PrincIf "Replaced Block Reference.")
    newObj
)

;; INSERTs a block reference onto the drawing
;; @param doc [#<VLA-OBJECT IAcadDocument>] a document object
;; @param chooseSpace [SYM] MODEL or PAPER
;; @param filepath [STR] can either be an actual filepath, or the name of a block reference in your AutoCAD block library
;; @returns [#<VLA-OBJECT IAcadBlockReference] the inserted block reference
(defun InsertBlockReference (doc chooseSpace filepath / choice blk)
    (PrintIf "Start InsertBlockReference -- ")
    (setq choice
        (cond
            ((eq chooseSpace 'PAPER)(vla-get-PaperSpace doc))
            ((eq chooseSpace 'MODEL)(vla-get-ModelSpace doc))
        ))
    (setq blk
        (vla-InsertBlock
            choice
            (vlax-3d-point 0 0 0)
            filepath
            1.0
            1.0
            1.0
            0.0
        )
    )
    (PrincIf "Block reference inserted.")
    blk
)

;; Copies attribute values from one block reference to another
;; @param oldObj [#<VLA-OBJECT IAcadBlockReference] source block reference
;; @param newobj [#<VLA-OBJECT IAcadBlockReference] target block reference
(defun CopyBlockAttributes (oldObj newObj / oldMap att val)
    (PrincIf "Start CopyBlockAttributes -- ")
    
    ;; Build a TAG -> VALUE map for old block
    (setq oldMap nil)
    (foreach att (vlax-invoke oldObj 'GetAttributes)
        (setq oldMap
            (cons
                (cons (strcase (vla-get-TagString att)) (vla-get-TextString att))
                oldMap
            )
        )
    )
    ;; Apply values of map to matching tags in new block
    (foreach att (vlax-invoke newObj 'GetAttributes)
        (if (setq val (cdr (assoc (strcase (vla-get-TagString att)) oldMap )))
            (progn
                (vla-put-TextString att val)
                ; (vla-put-ScaleFactor att 0.1)
                ; (vla-put-Alignment att acAlignmentFit)
                (vla-Update att)
            )
        )
    )

    (vla-Update newObj)
    (PrincIf "Done copying block attributes.")
    (princ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MULTI-FILE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Returns a list of all .dwg file paths as strings in a given directory
;; @param folder [STR] - a file directory
;; @returns [LIST] a list of strings
(defun GetAllDwgs (folder / result item fullpath)
    (PrintIf "Start GetAllDwgs -- ")
	; Initialize list of DWG file paths
	(setq result '())

	; Add all DWG files in the current folder
	(foreach item (vl-directory-files folder "*.dwg" 1)
		(setq result (cons (strcat folder "\\" item) result))
	)

	; Recursively process all subfolders
	(foreach item (vl-directory-files folder nil -1)
		; Ignore the current and parent directory entries
		(if (and (/= item ".") (/= item ".."))
			(setq fullpath (strcat folder "\\" item)
				; Append DWGs found in this subfolder
				result (append result (GetAllDwgs fullpath))
			)
		)
	)
	(PrincIf (strcat "\nFound " (itoa (length result)) " DWGs in directory " folder "\n"))
	result
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DEBUGGING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Wrapper for the princ function. Only runs princ if *DEBUG_MODE* is set to T
;; @param str [STR] string to princ
(defun PrincIf (str /)
	(if *DEBUG_MODE* (princ str))
	(princ)
)
;; Wrapper for the print function. Only runs print if *DEBUG_MODE* is set to T
;; @param str [STR] string to print
(defun PrintIf (str /)
	(if *DEBUG_MODE* (print str))
	(princ)
)