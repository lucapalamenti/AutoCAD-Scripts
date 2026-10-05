;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IMPORTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(vl-load-com)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OBJECT GETTERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Returns the active document
;; @returns [#<VLA-OBJECT IAcadDocument>]
#|
    @param dsadsadsaa
|#
(defun GetActiveDoc (/) (vla-get-ActiveDocument (vlax-get-acad-object)))

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
(defun c:SetupPageLayout (/)
    (SetupPageLayout (GetActiveDoc))
    (princ)
)
;; Configures the size and positioning of a page
;; @param doc [#<VLA-OBJECT IAcadDocument>] a document object
(defun SetupPageLayout (doc / lay)
    (PrintIf "Start SetupPageLayout -- ")
    ; Make sure doc is defined
    (if (null doc)
        (setq doc (GetActiveDoc))
    )

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
(defun c:FixViewport (/)
    (FixViewport (GetActiveDoc))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BLOCK REFERENCE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Replaces and copies all attributes from one block reference to a new one, returning the new object
;; @param doc [#<VLA-OBJECT IAcadDocument>] a document object
;; @param chooseSpace [SYM] MODEL or PAPER
;; @param oldObj [#<VLA-OBJECT IAcadBlockReference]
;; @param newObjFilePath [STR] File path to a .dwg file that contains a block reference
;; @returns [#<VLA-OBJECT IAcadBlockReference] the new block reference
(defun ReplaceBlockReference (doc chooseSpace oldObj newObjFilePath / newObj)
    (setq newObj (InsertBlockReference doc chooseSpace newObjFilePath))
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
        (if (setq val
                (cdr (assoc (strcase (vla-get-TagString att)) oldMap ))
            )
            (vla-put-TextString att val)
        )
    )

    (vla-Update newObj)
    (PrincIf "Done copying block attributes.")
    (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TEXT OBJECT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Converts all TEXT objects to MTEXT
(defun c:Text2MText (/ ss)
    (setq ss (ssget "_X" '((0 . "TEXT"))))
    (sssetfirst nil ss)
    (if ss
        (progn
            (command "_.TXT2MTXT" ss "")
            (PrincIf "All TEXT objects converted to MTEXT.")
        )
        (PrincIf "No TEXT objects found in the drawing.")
    )
    (princ)
)

;; Runs all text masking functions
(defun c:MaskText (/)
    (MaskAllMText)
    (MaskAllMLeaders)
    (c:DeleteWipeouts)
    (princ)
)
;; Applies a background color mask to all MTEXT objects
(defun MaskAllMText (/ ss i dxf_ent)
    (if (setq ss (ssget "_X" '((0 . "MTEXT"))))
        (repeat (setq i (sslength ss))
            (setq dxf_ent (entget (ssname ss (setq i (1- i)))))
            ; Remove old DXF codes (90, 63, 45, 40, 41) if they exist 
            (setq dxf_ent (vl-remove-if '(lambda (x) (member (car x) '(90 63 45 40 41))) dxf_ent))
            ; Add new DXF codes for background fill, color, padding, and width
            ; 90 = Background fill flag
            ; 63 = Background color
            ; 45 = Text padding
            ; 40 = Width
            ; 41 = Height
            (setq dxf_ent (append dxf_ent '((90 . 3) (63 . 256) (45 . 1.5) (41 . 0))))
            (entmod dxf_ent)
            (entupd (ssname ss i))
        )
        (PrincIf "No MText objects found.")
    )
    (princ)
)
;; Applies a background color mask to all MLEADER objects
(defun MaskAllMLeaders (/ ss i ent obj)
    (if (setq ss (ssget "_X" '((0 . "MULTILEADER"))))
        (progn
            (setq i 0)
            (repeat (sslength ss)
                (setq ent (ssname ss i))
                (setq obj (vlax-ename->vla-object ent))

                ; Enable background mask
                (if (vlax-property-available-p obj 'TextBackgroundFill)
                    (vla-put-TextBackgroundFill obj :vlax-true)
                )

                (setq i (1+ i))
            )
        )
        (PrincIf "No MLEADER objects found.")
    )
    (princ)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MULTI-FILE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Applies the given function "Work" to all .dwg files in the given folder. 
;; @param folder [STR]
;; @param Work [SYM]
;; @param args [LIST]
(defun ApplyToAll (folder Work args / docs doc dwgPath)
	(PrintIf "Start ApplyToAll -- ")
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

;; Returns a list of all .dwg file paths as strings in a given directory
;; @param folder [STR] - a file directory
;; @returns [LIST] a list of strings
(defun GetAllDwgs (folder / result item fullpath)
    (PrintIf "Start GetAllDwgs -- ")
	; Initialize list of DWG file paths
	(setq result '())

	; Add all DWG files in the current folder
	(foreach item (vl-directory-files folder "*.dwg" 1)
		(setq result (cons (strcat folder item) result))
	)

	; Recursively process all subfolders
	(foreach item (vl-directory-files folder nil -1)
		; Ignore the current and parent directory entries
		(if (and (/= item ".") (/= item ".."))
			(setq fullpath (strcat folder item)
				; Append DWGs found in this subfolder
				result (append result (GetAllDwgs fullpath))
			)
		)
	)
	(PrincIf (strcat "\nFound " (itoa (length result)) " DWGs in directory " folder "\n"))
	result
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MISC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Returns the number of objects in the active drawing
(defun c:CountAllObjects (/)
	(CountAllObjects (GetActiveDoc))
	(princ)
)
;; Returns the number of objects in the given document
;; @param doc [#<VLA-OBJECT IAcadDocument>] a document object
;; @returns [INT]
(defun CountAllObjects (doc / count obj)
	(setq count 0)
	;; ModelSpace
	(vlax-for obj (vla-get-ModelSpace doc)
		(setq count (1+ count))
	)
	;; PaperSpace
	(vlax-for obj (vla-get-PaperSpace doc)
		(setq count (1+ count))
	)
	(PrincIf (strcat "\nTotal objects found: " (itoa count)))
	count
)

;; Returns a list of file names from the provided file
;; @param filename [STR] a file path
;; @returns [LIST] a list of strings
(defun FileToList (filename / fp result line)
    ; Initialize list of file paths
    (setq result '())

    (if (setq fp (open filename "r"))
        (progn
            (while (setq line (read-line fp))
                (setq result
                    (cons
                        (strcase (vl-string-trim " " line))
                        result
                    )
                )
            )
            (close fp)
        )
    )
    (PrincIf (strcat "\nFound " (itoa (length result)) " names in " filename ":\n"))
    (reverse result)
)

;; Pauses the script for a given time
;; @params secs [INT] the number of seconds to pause the script for
(defun WaitSeconds (secs / endTime)
    (setq endTime (+ (getvar "DATE") (/ secs 86400.0)))
    (while (< (getvar "DATE") endTime))
)

;; Deletes all WIPEOUT objects
(defun c:DeleteWipeouts (/ ss)
    (if (setq ss (ssget "_X" '((0 . "WIPEOUT"))))
        (command "_.ERASE" ss "")
    )
    (princ)
)

;; Organizes draw order based on object properties:
;; - All XREFS to the back 
;; - All MTEXT objects to the front
;; - All geometry objects (circles, polylines, and lines) to the front
(defun c:OrderObjects (/ ss)
	; send all external references to the back of the draw order
	(if (setq ss (ssget "_X" '((0 . "XREF"))))
        (command "_.DRAWORDER" ss "" "B")
		(PrincIf "\nNo external references found in the drawing.")
	)
	
	; send all geometry objects to the front of the draw order
	(if (setq ss (ssget "_X" '((0 . "LWPOLYLINE,POLYLINE,LINE"))))
		(command "_.DRAWORDER" ss "" "F")
		(PrincIf "\nNo geometry objects found in the drawing.")
	)
	
	; send all MTEXT objects to the front of the draw order
	(if (setq ss (ssget "_X" '((0 . "MTEXT"))))
		(command "_.DRAWORDER" ss "" "F")
		(PrincIf "\nNo MTEXT objects found in the drawing.")
	)
	
	; send all Circle objects to the front of the draw order
	(if (setq ss (ssget "_X" '((0 . "CIRCLE"))))
		(command "_.DRAWORDER" ss "" "F")
		(PrincIf "\nNo geometry objects found in the drawing.")
	)

	(princ)
)

;; Moves all objects in Paper Space to Model Space, except the Block Reference template
(defun c:MoveObjectsToModel (/ ss ent vpSS)
	(command "_.PSPACE")
	; Select all objects in Paper Space
	; DXF 67 = workspace (0 = MODEL, 1 = PAPER)
	(if (setq ss (ssget "_X" '((67 . 1))))
		; Change space from Paper to Model
		(progn
		; remove the title block from the selection set
			(if (setq ent (GetTitleBlock))
				(ssdel ent ss)
			)
			; remove all viewports from the selection set
			(if (setq vpSS (ssget "_X" '((67 . 1) (0 . "VIEWPORT"))))
				(repeat (sslength vpSS)
					(ssdel (ssname vpSS 0) ss)
				)
			)

			(command "_.CHSPACE" ss "" "")
		)
		(PrincIf "\nNo objects found in Paper Space (or only Block References).")
	)
	(command "_.MSPACE")
	(princ)
)

;; Applies text styling to all MTEXT objects
(defun c:StyleMText (/ ss i ent obj)
	; select all MTEXT objects 
	(if (setq ss (ssget "_X" '((0 . "MTEXT"))))
		(progn
			(setq i 0)
			; repeat "length of ss (selection set)" times
			(repeat (sslength ss)
				(setq ent (ssname ss i))
				(setq obj (vlax-ename->vla-object ent))
				
				; Change any "STANDARD" font styles to "ROMANS"
				(if (wcmatch (strcase (vla-get-StyleName obj)) "STANDARD")
					(vla-put-StyleName obj "ROMANS")
				)

				; Set font size based on font style
				; If font style is Romans, set to 24; otherwise set to 48
				(if (wcmatch (strcase (vla-get-StyleName obj)) "ROMANS")
					(vla-put-Height obj 24)
					(vla-put-Height obj 48)
				)
				(setq i (1+ i))
			)
		)
		(PrincIf "\nNo MTEXT objects found.")
	)

	(princ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UTILITY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Separates a string using a given delimiter
;; str - [str] String to process
;; del - [str] Delimiter by which to separate the string
;; Returns: [lst] List of strings
(defun Str->List (str del / pos)
    (if (setq pos (vl-string-search del str))
        (cons (substr str 1 pos) (Str->List (substr str (+ pos 1 (strlen del))) del))
        (list str)
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DEBUGGING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Prints out DXF codes for a selected object to the command line
;; Mainly useful for debugging and understanding how to manipulate object properties with DXF codes
(defun c:ShowDXF (/ e)
    (if (setq e (car (entsel "\nSelect object: ")))
        (foreach x (entget e)
            (print x)
        )
    )
    (princ)
)

(defun c:GetName (/ ent)
    (if (setq ent (car (entsel "\nSelect an entity: ")))
        (princ (vla-get-ObjectName (vlax-ename->vla-object ent)))
    )
    (princ)
)

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