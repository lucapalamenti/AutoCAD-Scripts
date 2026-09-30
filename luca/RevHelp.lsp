;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OVERVIEW
;;; --------
;;; Helper function for drawing redlines/revisions.
;;;
;;; ** Any time this file is changed it MUST be reloaded in AutoCAD **
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IMPORTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GLOBAL VARIABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Change DefaultValue or create a new global variable if you need to
(setq *DefaultValue* 1)
(setq *PanelDWGValue* 0.003)
(setq *NADWGValue* 0.003)
(setq *BMSShopDWGValue* 0.6)

;; CHANGE THIS TO THE DESIRED GLOBAL VARIABLE ABOVE, OR ANY NUMERICAL VALUE
(setq *SelectedValue* *PanelDWGValue*)
;; CHANGE THIS TO YOUR RevTriangle.dwg FILEPATH
(setq *RevTriangleFilePath* "C:\\Users\\luca.palamenti\\OneDrive - RoviSys\\Documents\\AutoCAD\\RevTriangle.dwg")
;; CHANGE THIS IF YOU HAVE AN EXISTING REVISION LAYER YOU WANT TO USE (ONLY RECOMMENDED IF YOU KNOW YOUR LAYER WORKS)
(setq *RevLayerName* "RevClouds")
;; CHAGNE THISTO THE CURRENT REVISION
(setq *RevNumber* "0.1")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MAIN FUNCTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Places the RevTriangle.dwg object at a selected point.
;; Adds the triangle to the *RevLayerName* layer
(defun c:RevTriangle (/ doc ms insPt blkRef attObj minPt maxPt blkHeight p1 p2 lineObj)
	(SetLayerToTrueRGB *RevLayerName* 255 0 0)
	; Prompt user for insertion point
	(setq insPt (getpoint "\nSelect insertion point for RevTriangle: "))
	(if insPt
		(progn
		; Get modelspace
			(setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
			(setq ms  (vla-get-ModelSpace doc))
			; Insert RevTriangle Block Reference
			(setq blkRef (vla-InsertBlock ms (vlax-3d-point insPt) *RevTriangleFilePath* *SelectedValue* *SelectedValue* *SelectedValue* 0.0))

			; Set Revision attribute
			(if (= (vla-get-HasAttributes blkRef) :vlax-true)
				(foreach attObj (vlax-invoke blkRef 'GetAttributes)
					(if (= (strcase (vla-get-TagString attObj)) "REV")
						(vla-put-TextString attObj *RevNumber*)
					)
				)
			)

			;; Add extra line object to bottom of RevTriangle

			; Get block extents
			(vla-GetBoundingBox blkRef 'minPt 'maxPt)

			(setq minPt (vlax-safearray->list minPt))
			(setq maxPt (vlax-safearray->list maxPt))

			; Height of block
			(setq blkHeight (- (cadr maxPt) (cadr minPt)))

			; Bottom center point
			(setq p1 (list (/ (+ (car minPt) (car maxPt)) 2.0) (cadr minPt) 0.0))
			; Extend downward one block height
			(setq p2 (list (car p1) (- (cadr p1) blkHeight) 0.0))

			; Draw line
			(setq lineObj (vla-AddLine ms (vlax-3d-point p1) (vlax-3d-point p2)))
			(vla-put-layer lineObj *RevLayerName*)
			; Set color to ByLayer
			(vla-put-color lineObj 256)
		
		)
	)

	(princ)
)

;; "Reacts" to typing the command REVCLOUD.
;; Automatically sets the color to ByLayer and object layer to *RevLayerName*
(setq *RevCloudReactorVar* (vlr-command-reactor nil '((:vlr-commandEnded . RevCloudReactor))))
(defun RevCloudReactor (reactor params / ent obj)
	(if (= (strcase (car params)) "REVCLOUD")
		(progn
			(SetLayerToTrueRGB *RevLayerName* 255 0 0)

			(setq ent (entlast))
			(setq obj (vlax-ename->vla-object ent))
			; Set color to ByLayer
			(vla-put-color obj 256)
			; Set the object layer to *RevLayerName*
			(vla-put-layer obj *RevLayerName*)
		)
	)
)

;; Checks if an object layer exists. If it doesn't then create it.
;; @param layerName [string] - the name of the layer to check
;; @returns [VLA-OBJECT] - the existing/created layer
(defun EnsureLayer (layerName / doc lays lay)
	(setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
	(setq lays (vla-get-Layers doc))
	(if (not(tblsearch "LAYER" layerName))
		(vla-Add lays layerName)
	)
	(setq lay (vla-item lays layerName))
	lay
)

;; Sets a layer to use the "TrueColor" with the given RGB values
;; @param r [Integer] - number from 0-255 inclusive
;; @param g [Integer] - number from 0-255 inclusive
;; @param b [Integer] - number from 0-255 inclusive
(defun SetLayerToTrueRGB ( layerName r g b / acadObj doc layers layerObj trueColorObj)
	(setq acadObj (vlax-get-acad-object)
			doc     (vla-get-activedocument acadObj)
			layers  (vla-get-layers doc)
	)

	; Make sure the layer exists first
	(setq layerObj (EnsureLayer layerName))
	
	(progn
		; Fetch the embedded TrueColor structure from the layer object
		(setq trueColorObj (vla-get-truecolor layerObj))
		
		; Update the color values inside the structure to r, g, b
		(vla-setrgb trueColorObj r g b)
		
		; Write the updated color structure back to the layer definition
		(vla-put-truecolor layerObj trueColorObj)
		(princ (strcat "\nLayer " layerName " updated to True Red (VLA)."))
	)
	(princ)
)