;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IMPORTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(vl-load-com)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OBJECT GET FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Returns the active document
;; @returns [#<VLA-OBJECT IAcadDocument>]
(defun GetActiveDoc (/) (vla-get-ActiveDocument (vlax-get-acad-object)))

;; Returns the layout tab at the given index for a given document. "Model" is always index 0.
;; @param doc [#<VLA-OBJECT IAcadDocument>] a document object
;; @param index [INT] tab index
;; @returns [#<VLA-OBJECT IAcadLayout>]
(defun GetLayoutTab (doc index / lay layout)
    (vlax-for lay (vla-get-Layouts doc)
        (if (= (vla-get-TabOrder lay) index)
            (setq layout (vla-item (vla-get-Layouts doc) "Layout1"))
        )
    )
    layout
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PAGE SETUP FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Configures the size and positioning of a page
(defun c:SetupPageLayout (/)
    (SetupPageLayout (GetActiveDoc))
    (princ)
)
;; Configures the size and positioning of a page
;; @param doc [#<VLA-OBJECT IAcadDocument>] a document object
(defun SetupPageLayout (doc / lay)
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

    (princ)
)

;; Make sure the VIEWPORT is ordered, centered, & sized correctly
(defun c:FixViewport (/ ss ent obj)
    (FixViewport (GetActiveDoc))
    (princ)
)
;; Make sure the VIEWPORT is ordered, centered, & sized correctly
;; @param doc [#<VLA-OBJECT IAcadDocument>] a document object
(defun FixViewport (doc / viewport)
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

    (princ "\nViewport properties updated: Center (8.5,5.5), Height 10.5, Width 16.5, moved to front.")
    (princ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BLOCK REFERENCE FUNCTIONS
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
    (princ "Replaced Block Reference\n")
    newObj
)

;; INSERTs a block reference onto the drawing
;; @param doc [#<VLA-OBJECT IAcadDocument>] a document object
;; @param chooseSpace [SYM] MODEL or PAPER
;; @param filepath [STR] can either be an actual filepath, or the name of a block reference in your AutoCAD block library
;; @returns [#<VLA-OBJECT IAcadBlockReference] the inserted block reference
(defun InsertBlockReference (doc chooseSpace filepath / space)
    (setq chooseSpace
        (cond
            ((eq chooseSpace 'PAPER)(vla-get-PaperSpace doc))
            ((eq chooseSpace 'MODEL)(vla-get-ModelSpace doc))
        ))
    (vla-InsertBlock
        chooseSpace
        (vlax-3d-point 0 0 0)
        filepath
        1.0
        1.0
        1.0
        0.0
    )
)

;; Copies attribute values from one block reference to another
;; @param oldObj [#<VLA-OBJECT IAcadBlockReference] source block reference
;; @param newobj [#<VLA-OBJECT IAcadBlockReference] target block reference
(defun CopyBlockAttributes (oldObj newObj / oldMap val)
    (princ "Copying block attributes\n")
    
    ;; Build a TAG -> VALUE map for old block
    (setq oldMap nil)
    (foreach att (vlax-invoke oldObj 'GetAttributes)
        (setq oldMap
            (cons
                (cons (strcase (vla-get-TagString att))
                    (vla-get-TextString att))
                oldMap
            )
        )
    )
    ;; Apply values of map to matching tags in new block
    (foreach att (vlax-invoke newObj 'GetAttributes)
        (if (setq val
                (cdr
                    (assoc
                        (strcase (vla-get-TagString att))
                        oldMap
                    )
                )
            )
            (vla-put-TextString att val)
        )
    )

    (vla-Update newObj)
    (princ "Done copying block attributes\n")
    (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TEXT FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Converts all TEXT objects to MTEXT
(defun c:Text2MText (/ ss)
    (setq ss (ssget "_X" '((0 . "TEXT"))))
    (sssetfirst nil ss)
    (if ss
        (progn
            (command "_.TXT2MTXT" ss "")
            (princ "\nAll TEXT objects converted to MTEXT.")
        )
        (princ "\nNo TEXT objects found in the drawing.")
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
        (princ "\nNo MText objects found.")
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
        (princ "\nNo MLEADER objects found.")
    )
    (princ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DEBUGGING FUNCTIONS
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MULTI-FILE FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Applies the given function "Work" to all .dwg files in the given folder. 
;; @param folder [STR]
;; @param Work [SYM]
;; @param args [LIST]
(defun ApplyToAll (folder Work args / docs doc dwgPath)
	(princ "Start ApplyToAll function.\n")
	(setq docs (vla-get-Documents (vlax-get-acad-object)))
	
	(foreach dwgPath (GetAllDwgs folder)
		; Open drawing
		(princ (strcat "Opening: " (vl-filename-base dwgPath) "\n"))
		(setq doc (vla-open docs dwgPath))
		
		; Call Work regardless of arguments
		(apply Work (cons doc args))
		
		; Save and close
		(princ (strcat "Closing: " (vl-filename-base dwgPath) "\n"))
		(vla-save doc)
		(vla-close doc)
	)
	(princ "End ApplyToAll function.\n")
)

;; Returns a list of all .dwg file paths as strings in a given directory
;; @param folder [STR] - a file directory
;; @returns [LIST] a list of strings
(defun GetAllDwgs (folder / result item fullpath)
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
	(princ (strcat "\nFound " (itoa (length result)) " DWGs in directory " folder "\n"))
	result
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MISC FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Returns the number of objects in the active drawing
(defun c:CountAllObjects (/)
	(CountAllObjects (GetActiveDoc))
	(princ)
)
;; Returns the number of objects in the given document
;; @param doc [#<VLA-OBJECT IAcadDocument>] a document object
;; @returns [INT]
(defun CountAllObjects (doc / count)
	(setq count 0)
	;; ModelSpace
	(vlax-for obj (vla-get-ModelSpace doc)
		(setq count (1+ count))
	)
	;; PaperSpace
	(vlax-for obj (vla-get-PaperSpace doc)
		(setq count (1+ count))
	)
	(princ (strcat "\nTotal objects found: " (itoa count)))
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
    (princ (strcat "\nFound " (itoa (length result)) " names in " filename ":\n"))
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
	; select all external reference objects in the drawing
	(if (setq ss (ssget "_X" '((0 . "XREF"))))
		(progn
		; send all external references to the back of the draw order
			(command "_.DRAWORDER" ss "" "B")
			(princ "\nAll external references sent to back of draw order.")
		)
		(princ "\nNo external references found in the drawing.")
	)
	
	; select geometry objects (polylines, and lines)
	(if (setq ss (ssget "_X" '((0 . "LWPOLYLINE,POLYLINE,LINE"))))
		(progn
		; send all geometry objects to the front of the draw order
			(command "_.DRAWORDER" ss "" "F")
			(princ "\nAll geometry objects sent to front of draw order.")
		)
		(princ "\nNo geometry objects found in the drawing.")
	)
	
	; select all MTEXT objects in the drawing
	(if (setq ss (ssget "_X" '((0 . "MTEXT"))))
		(progn
		; send all MTEXT objects to the front of the draw order
			(command "_.DRAWORDER" ss "" "F")
			(princ "\nMTEXT objects sent to front of draw order.")
		)
		(princ "\nNo MTEXT objects found in the drawing.")
	)
	
	; select all Circle objects
	(if (setq ss (ssget "_X" '((0 . "CIRCLE"))))
		(progn
		; send all Circle objects to the front of the draw order
			(command "_.DRAWORDER" ss "" "F")
			(princ "\nAll geometry objects sent to front of draw order.")
		)
		(princ "\nNo geometry objects found in the drawing.")
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
			(princ "\nObjects moved from Paper Space to Model Space.")
		)
		(princ "\nNo objects found in Paper Space (or only Block References).")
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
			(princ (strcat "\nFormatted " (itoa (sslength ss)) " MTEXT objects."))
		)
		(princ "\nNo MTEXT objects found.")
	)

	(princ)
)
