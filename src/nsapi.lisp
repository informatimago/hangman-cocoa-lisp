;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               nsapi.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines a lisp layer above Cocoa.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-06-17 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2015
;;;;    
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Affero General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Affero General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU Affero General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************
(in-package "COM.INFORMATIMAGO.HANGMAN.COCOA")
(objcl:set-objective-cl-syntax)

(defstruct (point (:type list))
  x y)
(defstruct (size (:type list))
  width height)
(defstruct (rect (:type list))
  x y width height)

(defmacro dovector ((var vector &optional result) &body body)
  (let ((vvector (gensym "vector"))
        (vindex  (gensym "index"))
        (vlength (gensym "length")))
    `(block nil
       (let* ((,vvector ,vector)
              (,vlength (length ,vvector))
              (,vindex  -1))
         (tagbody
            (go :test)
          :loop
            (let ((,var (aref ,vvector ,vindex)))
              ,@body)
          :test
            (incf ,vindex)
            (if (< ,vindex ,vlength)
                (go :loop))
            (return ,result))))))

(declaim (inline nsrect nspoint nssize nsarray))
(defun nsrect (frame)
  (apply (function ns:make-ns-rect) frame))
(defun nspoint (point)
  (apply (function ns:make-ns-point) point))
(defun nssize (size)
  (apply (function ns:make-ns-size) size))
(defun nsarray (sequence)
  (etypecase sequence
    (ns:ns-array sequence)
    (sequence    (let ((array [NSMutableArray arrayWithCapacity:(length sequence)]))
                   (map nil (lambda (element) [array addObject:element])
                     sequence)
                   array))))
(defun nsstring (string)
  (etypecase string
    (ns:ns-string string)
    (string (objcl:objc-string string))
    (symbol (objcl:objc-string (symbol-name string)))))
(defun nsurl (url)
  (typecase url
    (ns:ns-url url)
    (t         [NSURL URLWithString:(nsstring url)])))

(declaim (inline nstimeinterval cgfloat sfloat fontsize coord nsbool))
(defun nstimeinterval (value) (coerce value 'double-float))
(defun cgfloat        (value) (coerce value 'ns:cgfloat))
(defun sfloat         (value) (coerce value 'single-float))
(defun fontsize       (value) (values (round  value)))
(defun coord          (value) (values (round  value)))
(defconstant yes #$YES)
(defconstant no  #$NO)
(defun nsbool         (value) (case value
                                ((1) yes)
                                ((0) no)
                                ((nil) no)
                                (otherwise yes)))


(defun ns-matrix-mode (mode)
  (ecase mode
    ((:radio     #.#$NSRadioModeMatrix)     #$NSRadioModeMatrix)
    ((:highlight #.#$NSHighlightModeMatrix) #$NSHighlightModeMatrix)
    ((:list      #.#$NSListModeMatrix)      #$NSListModeMatrix)
    ((:track     #.#$NSTrackModeMatrix)     #$NSTrackModeMatrix)))
    
(defun ns-button-type (type)
  (ecase type
    ((:momentary-push :momentary-light
                      #.#$NSMomentaryLightButton)     #$NSMomentaryLightButton)   
    ((:push-on-push-off  #.#$NSPushOnPushOffButton)   #$NSPushOnPushOffButton)    
    ((:toggle            #.#$NSToggleButton)          #$NSToggleButton)           
    ((:switch            #.#$NSSwitchButton)          #$NSSwitchButton)           
    ((:radio             #.#$NSRadioButton)           #$NSRadioButton)            
    ((:momentary-change  #.#$NSMomentaryChangeButton) #$NSMomentaryChangeButton)  
    ((:on-off            #.#$NSOnOffButton)           #$NSOnOffButton)            
    ((:momentary-push-in #.#$NSMomentaryPushInButton) #$NSMomentaryPushInButton)  
    #+cocoa-10.10 ((:accelerator             #.#$NSAcceleratorButton)           #$NSAcceleratorButton)
    #+cocoa-10.10 ((:multi-level-accelerator #.#$NSMultiLevelAcceleratorButton) #$NSMultiLevelAcceleratorButton)))
    
(defun ns-bezel-style (style)
  (ecase style
    ((:rounded            #.#$NSRoundedBezelStyle)           #$NSRoundedBezelStyle)             
    ((:regular-square     #.#$NSRegularSquareBezelStyle
                          :small-icon-button)                #$NSRegularSquareBezelStyle)       
    ((:thick-square       #.#$NSThickSquareBezelStyle)       #$NSThickSquareBezelStyle)         
    ((:thicker-square     #.#$NSThickerSquareBezelStyle)     #$NSThickerSquareBezelStyle)       
    ((:disclosure         #.#$NSDisclosureBezelStyle)        #$NSDisclosureBezelStyle)          
    ((:shadowless         #.#$NSShadowlessSquareBezelStyle)  #$NSShadowlessSquareBezelStyle)    
    ((:circular           #.#$NSCircularBezelStyle)          #$NSCircularBezelStyle)            
    ((:textured-square    #.#$NSTexturedSquareBezelStyle)    #$NSTexturedSquareBezelStyle)      
    ((:help-button        #.#$NSHelpButtonBezelStyle)        #$NSHelpButtonBezelStyle)          
    ((:small-square       #.#$NSSmallSquareBezelStyle)       #$NSSmallSquareBezelStyle)         
    ((:textured-rounded   #.#$NSTexturedRoundedBezelStyle)   #$NSTexturedRoundedBezelStyle)     
    ((:round-rect         #.#$NSRoundRectBezelStyle)         #$NSRoundRectBezelStyle)           
    ((:recessed           #.#$NSRecessedBezelStyle)          #$NSRecessedBezelStyle)            
    ((:rounded-disclosure #.#$NSRoundedDisclosureBezelStyle) #$NSRoundedDisclosureBezelStyle)   
    #+cocoa-10.10 ((:inline #.#$NSInlineBezelStyle)          #$NSInlineBezelStyle)))
    
(defun ns-gradient-type (type)
  (ecase type
    ((:none           #.#$NSGradientNone)          #$NSGradientNone)          
    ((:concave-weak   #.#$NSGradientConcaveWeak)   #$NSGradientConcaveWeak)   
    ((:concave-strong #.#$NSGradientConcaveStrong) #$NSGradientConcaveStrong)
    ((:convex-weak    #.#$NSGradientConvexWeak)    #$NSGradientConvexWeak)    
    ((:convex-strong  #.#$NSGradientConvexStrong)  #$NSGradientConvexStrong)))
    
(defun ns-text-field-bezel-style (bezel-style)
  (ecase bezel-style
    ((:square  #.#$NSTextFieldSquareBezel)  #$NSTextFieldSquareBezel)
    ((:rounded #.#$NSTextFieldRoundedBezel) #$NSTextFieldRoundedBezel)))
    
(defun ns-view-layer-contents-placement (placement)
  (ecase placement
    ((:scale-axes-independently     #.#$NSViewLayerContentsPlacementScaleAxesIndependently)    #$NSViewLayerContentsPlacementScaleAxesIndependently)      
    ((:scale-proportionally-to-fit  #.#$NSViewLayerContentsPlacementScaleProportionallyToFit)  #$NSViewLayerContentsPlacementScaleProportionallyToFit)    
    ((:scale-proportionally-to-fill #.#$NSViewLayerContentsPlacementScaleProportionallyToFill) #$NSViewLayerContentsPlacementScaleProportionallyToFill)   
    ((:center                       #.#$NSViewLayerContentsPlacementCenter)                    #$NSViewLayerContentsPlacementCenter)                      
    ((:top                          #.#$NSViewLayerContentsPlacementTop)                       #$NSViewLayerContentsPlacementTop)                         
    ((:top-right                    #.#$NSViewLayerContentsPlacementTopRight)                  #$NSViewLayerContentsPlacementTopRight)                    
    ((:right                        #.#$NSViewLayerContentsPlacementRight)                     #$NSViewLayerContentsPlacementRight)                       
    ((:bottom-right                 #.#$NSViewLayerContentsPlacementBottomRight)               #$NSViewLayerContentsPlacementBottomRight)                 
    ((:bottom                       #.#$NSViewLayerContentsPlacementBottom)                    #$NSViewLayerContentsPlacementBottom)                      
    ((:bottom-left                  #.#$NSViewLayerContentsPlacementBottomLeft)                #$NSViewLayerContentsPlacementBottomLeft)                  
    ((:left                         #.#$NSViewLayerContentsPlacementLeft)                      #$NSViewLayerContentsPlacementLeft)                        
    ((:top-left                     #.#$NSViewLayerContentsPlacementTopLeft)                   #$NSViewLayerContentsPlacementTopLeft)))
    
(defun ns-view-layer-contents-redraw-policy (policy)
  (ecase policy
    ((:never                #.#$NSViewLayerContentsRedrawNever)             #$NSViewLayerContentsRedrawNever)                  
    ((:on-set-needs-display #.#$NSViewLayerContentsRedrawOnSetNeedsDisplay) #$NSViewLayerContentsRedrawOnSetNeedsDisplay)      
    ((:during-view-resize   #.#$NSViewLayerContentsRedrawDuringViewResize)  #$NSViewLayerContentsRedrawDuringViewResize)       
    ((:before-view-resize   #.#$NSViewLayerContentsRedrawBeforeViewResize)  #$NSViewLayerContentsRedrawBeforeViewResize)
    #+cocoa-10.10 ((:crossfade #.#$NSViewLayerContentsRedrawCrossfade) #$NSViewLayerContentsRedrawCrossfade)))

(defun ns-text-alignment (alignment)
  (ecase alignment
    ((:left      #.#$NSLeftTextAlignment)      #$NSLeftTextAlignment)
    ((:right     #.#$NSRightTextAlignment)     #$NSRightTextAlignment)
    ((:center    #.#$NSCenterTextAlignment)    #$NSCenterTextAlignment)
    ((:justified #.#$NSJustifiedTextAlignment) #$NSJustifiedTextAlignment)
    ((:natural   #.#$NSNaturalTextAlignment)   #$NSNaturalTextAlignment)))
    
(defun ns-writing-direction (direction)
  (ecase direction
    ((:natural       #.#$NSWritingDirectionNatural)     #$NSWritingDirectionNatural)
    ((:left-to-right #.#$NSWritingDirectionLeftToRight) #$NSWritingDirectionLeftToRight)
    ((:right-to-left #.#$NSWritingDirectionRightToLeft) #$NSWritingDirectionRightToLeft)))
    
(defun ns-cell-image-position (position)
  (ecase position
    ((:none     #.#$NSNoImage)       #$NSNoImage)       
    ((:only     #.#$NSImageOnly)     #$NSImageOnly)     
    ((:left     #.#$NSImageLeft)     #$NSImageLeft)     
    ((:right    #.#$NSImageRight)    #$NSImageRight)    
    ((:below    #.#$NSImageBelow)    #$NSImageBelow)    
    ((:above    #.#$NSImageAbove)    #$NSImageAbove)    
    ((:overlaps #.#$NSImageOverlaps) #$NSImageOverlaps)))
    
(defun ns-image-frame-style (style)
  (ecase style
    ((:none       #.#$NSImageFrameNone)      #$NSImageFrameNone) 
    ((:photo      #.#$NSImageFramePhoto)     #$NSImageFramePhoto)
    ((:gray-bezel #.#$NSImageFrameGrayBezel) #$NSImageFrameGrayBezel)
    ((:groove     #.#$NSImageFrameGroove)    #$NSImageFrameGroove)
    ((:button     #.#$NSImageFrameButton)    #$NSImageFrameButton)))
    
(defun ns-image-alignment (alignment)
  (ecase alignment
    ((:center       #.#$NSImageAlignCenter)      #$NSImageAlignCenter)
    ((:top          #.#$NSImageAlignTop)         #$NSImageAlignTop)
    ((:top-left     #.#$NSImageAlignTopLeft)     #$NSImageAlignTopLeft)
    ((:top-right    #.#$NSImageAlignTopRight)    #$NSImageAlignTopRight)
    ((:left         #.#$NSImageAlignLeft)        #$NSImageAlignLeft)
    ((:bottom       #.#$NSImageAlignBottom)      #$NSImageAlignBottom)
    ((:bottom-left  #.#$NSImageAlignBottomLeft)  #$NSImageAlignBottomLeft)
    ((:bottom-right #.#$NSImageAlignBottomRight) #$NSImageAlignBottomRight)
    ((:right        #.#$NSImageAlignRight)       #$NSImageAlignRight)))
    
(defun ns-image-scaling (scaling)
  (ecase scaling
    ((:proportionally-down       #.#$NSImageScaleProportionallyDown)     #$NSImageScaleProportionallyDown)     
    ((:axes-independently        #.#$NSImageScaleAxesIndependently)      #$NSImageScaleAxesIndependently)
    ((:none                      #.#$NSImageScaleNone)                   #$NSImageScaleNone)
    ((:proportionally-up-or-down #.#$NSImageScaleProportionallyUpOrDown) #$NSImageScaleProportionallyUpOrDown)))
    
(defun ns-window-style-mask (mask)
  (let ((result #$NSBorderlessWindowMask))
    (cond
      ((integerp mask)
       mask)
      ((or (null mask) (eq :borderless mask))
       result)
      ((atom mask)
       (ns-window-style-mask (list mask)))
      (t
       (when (member :titled              mask) (incf result #$NSTitledWindowMask))
       (when (member :closable            mask) (incf result #$NSClosableWindowMask))
       (when (member :miniaturizable      mask) (incf result #$NSMiniaturizableWindowMask))
       (when (member :resizable           mask) (incf result #$NSResizableWindowMask))
       (when (member :textured-background mask) (incf result #$NSTexturedBackgroundWindowMask))
       result))))

(defun ns-backing-store-type (type)
  (ecase type
    ((:retained     #.#$NSBackingStoreRetained)    #$NSBackingStoreRetained)
    ((:non-retained #.#$NSBackingStoreNonretained) #$NSBackingStoreNonretained)
    ((:buffered     #.#$NSBackingStoreBuffered)    #$NSBackingStoreBuffered)))

(defun ns-window-collection-behavior (behavior)
  (ecase behavior
    ((:default              #.#$NSWindowCollectionBehaviorDefault)           #$NSWindowCollectionBehaviorDefault)
    ((:can-join-all-spaces  #.#$NSWindowCollectionBehaviorCanJoinAllSpaces)  #$NSWindowCollectionBehaviorCanJoinAllSpaces)
    ((:move-to-active-space #.#$NSWindowCollectionBehaviorMoveToActiveSpace) #$NSWindowCollectionBehaviorMoveToActiveSpace)))

(defun ns-window-sharing-type (type)
  (ecase type
    ((:none       #.#$NSWindowSharingNone)      #$NSWindowSharingNone)      
    ((:read-only  #.#$NSWindowSharingReadOnly)  #$NSWindowSharingReadOnly)
    ((:read-write #.#$NSWindowSharingReadWrite) #$NSWindowSharingReadWrite)))




(defgeneric configure (object &key &allow-other-keys)
  (:documentation "Sets the object parameters.
The CONFIGURE method provides a lispier API to initialize the Cocoa Objects,
since it takes mostly lisp objects as parameter and convert them to the required NS types:
string -> NSString,
keywords -> NSInteger enums,
real -> CGFloat or whatever is needed
rect list -> NSRect
size list -> NSSize
point list -> NSPoint
"))

(defmethod configure ((self ns:ns-responder)
                      &key
                        (next-responder nil next-responder-p)
                        (menu nil menu-p)
                      &allow-other-keys)
  (when (next-method-p) (call-next-method))
  (when next-responder-p [self setNextResponder: next-responder])
  (when menu-p [self setMenu: menu])
  self)

(defmethod configure ((self ns:ns-view)
                      &key
                        (superview nil superview-p)
                        (subviews '()  subviews-p)
                        (frame nil frame-p)
                        (frame-rotation 0.0 frame-rotation-p)
                        (bounds nil bounds-p)
                        (bounds-rotation 0.0 bounds-rotation-p)
                        add-constraints
                        remove-constraints
                        (layer nil layer-p)
                        (wants-layer nil wants-layer-p)
                        (wants-update-layer nil wants-update-layer-p)
                        (layer-contents-placement nil layer-contents-placement-p)
                        (layer-contents-redraw-policy nil layer-contents-redraw-policy-p)
                        (can-draw-subviews-into-layer nil can-draw-subviews-into-layer-p)
                        (layer-uses-core-image-filters nil layer-uses-core-image-filters-p)
                        (alpha-value nil alpha-value-p)
                        (frame-center-rotation nil frame-center-rotation-p)
                        (background-filters nil background-filters-p)
                        (compositing-filter nil compositing-filter-p)
                        (content-filters nil content-filters-p)
                        (shadow nil shadow-p)
                      &allow-other-keys)
  (declare (ignorable
            add-constraints                
            remove-constraints             
            wants-update-layer             
            wants-update-layer-p           
            can-draw-subviews-into-layer   
            can-draw-subviews-into-layer-p 
            layer-uses-core-image-filters  
            layer-uses-core-image-filters-p))
  (when (next-method-p) (call-next-method))
  (when frame-p                         [self setFrame:(nsrect frame)])
  (when frame-rotation-p                [self setFrameRotation:(cgfloat frame-rotation)])
  (when bounds-p                        [self setBounds:(nsrect bounds)])
  (when bounds-rotation-p               [self setBoundsRotation:(cgfloat bounds-rotation)])
  (when subviews-p                      [self setSubviews:(nsarray subviews)])
  (when wants-layer-p                   [self setWantsLayer:(nsbool wants-layer)])
  #+cocoa-10.10 (when wants-update-layer-p            [self setWantsUpdateLayer:(nsbool wants-update-layer)])
  (when layer-p                         [self setLayer:layer])
  (when layer-contents-placement-p      [self setLayerContentsPlacement:(ns-view-layer-contents-placement layer-contents-placement)])
  (when layer-contents-redraw-policy-p  [self setLayerContentsRedrawPolicy:(ns-view-layer-contents-redraw-policy layer-contents-redraw-policy)])
  #+cocoa-10.10 (when can-draw-subviews-into-layer-p  [self setCanDrawSubviewsIntoLayer:(nsbool can-draw-subviews-into-layer)])
  #+cocoa-10.10 (when layer-uses-core-image-filters-p [self setLayerUsesCoreImageFilters:(nsbool layer-uses-core-image-filters)])
  (when alpha-value-p                   [self setAlphaValue:(cgfloat alpha-value)])
  (when frame-center-rotation-p         [self setFrameCenterRotation: (cgfloat frame-center-rotation)])
  (when background-filters-p            [self setBackgroundFilters:(nsarray background-filters)])
  (when compositing-filter-p            [self setCompositingFilter:(nsarray compositing-filter)])
  (when content-filters-p               [self setContentFilters:(nsarray content-filters)])
  (when shadow-p                        [self setShadow:shadow])
  #+cocoa-10.10 (when remove-constraints
                  [self removeConstraints:(nsarray remove-constraints)])
  #+cocoa-10.10 (when add-constraints
                  [self addConstraints:(nsarray add-constraints)])
  (when superview-p                      [superview addSubview:self])
  self)


(defmethod configure ((self ns:ns-control)
                      &key
                        (enabled nil enabled-p)
                        (double-value nil double-value-p)
                        (float-value nil float-value-p)
                        (int-value nil int-value-p)
                        (integer-value nil integer-value-p)
                        (object-value nil object-value-p)
                        (string-value nil string-value-p)
                        (attributed-string-value nil attributed-string-value-p)
                        (alignment nil alignment-p)
                        (font nil font-p)
                        (formatter nil formatter-p)
                        (base-writing-direction nil base-writing-direction-p)
                        (allows-expansion-tool-tips nil allows-expansion-tool-tips-p)
                        (action nil action-p)
                        (target nil target-p)
                        (continuous nil continuous-p)
                        (tag nil tag-p)
                        (refuses-first-responder nil refuses-first-responder-p)
                      &allow-other-keys)
  (declare (ignorable
            allows-expansion-tool-tips  
            allows-expansion-tool-tips-p))
  (when (next-method-p) (call-next-method))
  (when enabled-p [self setEnabled: (nsbool enabled)])
  (when double-value-p [self setDoubleValue: (coerce double-value 'double-float)])
  (when float-value-p [self setFloatValue: (coerce float-value 'single-float)])
  (when int-value-p [self setIntValue: (coerce int-value 'integer)])
  (when integer-value-p [self setIntegerValue: (coerce integer-value 'integer)])
  (when object-value-p [self setObjectValue: object-value])
  (when string-value-p [self setStringValue: (nsstring string-value)])
  (when attributed-string-value-p [self setAttributedStringValue: attributed-string-value])
  (when alignment-p [self setAlignment: (ns-text-alignment alignment)])
  (when font-p [self setFont: font])
  (when formatter-p [self setFormatter: formatter])
  (when base-writing-direction-p [self setBaseWritingDirection: (ns-writing-direction base-writing-direction)])
  #+cocoa-10.10 (when allows-expansion-tool-tips-p [self setAllowsExpansionToolTips: allows-expansion-tool-tips])
  (when action-p [self setAction: action])
  (when target-p [self setTarget: target])
  (when continuous-p [self setContinuous: (nsbool continuous)])
  (when tag-p [self setTag: tag])
  (when refuses-first-responder-p [self setRefusesFirstResponder: (nsbool refuses-first-responder)])
  self)


(defmethod configure ((self ns:ns-button)
                      &key
                        (button-type nil button-type-p)
                        (alternate-title nil alternate-title-p)
                        (attributed-title nil attributed-title-p)
                        (title nil title-p)
                        (sound nil sound-p)
                        (spring-loaded nil spring-loaded-p)
                        (max-accelerator-level nil max-accelerator-level-p)
                        (image nil image-p)
                        (alternate-image nil alternate-image-p)
                        (image-position nil image-position-p)
                        (bordered nil bordered-p)
                        (transparent nil transparent-p)
                        (bezel-style nil bezel-style-p)
                        (shows-border-only-while-mouse-inside nil shows-border-only-while-mouse-inside-p)
                        (allows-mixed-state nil allows-mixed-state-p)
                        (state nil state-p)
                        (key-equivalent nil key-equivalent-p)
                        (key-equivalent-modifier-mask nil key-equivalent-modifier-mask-p)
                      &allow-other-keys)
  (declare (ignore
            spring-loaded          
            spring-loaded-p        
            max-accelerator-level  
            max-accelerator-level-p))
  (when (next-method-p) (call-next-method))
  (when button-type-p [self setButtonType: (ns-button-type button-type)])
  (when alternate-title-p [self setAlternateTitle: (nsstring alternate-title)])
  (when attributed-title-p [self setAttributedTitle: attributed-title])
  (when title-p [self setTitle: (nsstring title)])
  (when sound-p [self setSound: sound])
  #+cocoa-10.10 (when spring-loaded-p [self setSpringLoaded: spring-loaded])
  #+cocoa-10.10(when max-accelerator-level-p [self setMaxAcceleratorLevel: max-accelerator-level])
  (when image-p [self setImage: image])
  (when alternate-image-p [self setAlternateImage: alternate-image])
  (when image-position-p [self setImagePosition: (ns-cell-image-position image-position)])
  (when bordered-p [self setBordered: (nsbool bordered)])
  (when transparent-p [self setTransparent: (nsbool transparent)])
  (when bezel-style-p [self setBezelStyle: (ns-bezel-style bezel-style)])
  (when shows-border-only-while-mouse-inside-p [self setShowsBorderOnlyWhileMouseInside: (nsbool shows-border-only-while-mouse-inside)])
  (when allows-mixed-state-p [self setAllowsMixedState: (nsbool allows-mixed-state)])
  (when state-p [self setState: state])
  (when key-equivalent-p [self setKeyEquivalent: (nsstring key-equivalent)])
  (when key-equivalent-modifier-mask-p [self setKeyEquivalentModifierMask: key-equivalent-modifier-mask])
  self)


(defmethod configure ((self ns:ns-image-view)
                      &key
                        (image nil image-p)
                        (image-frame-style nil image-frame-style-p)
                        (image-alignment nil image-alignment-p)
                        (image-scaling nil image-scaling-p)
                        (animates nil animates-p)
                        (editable nil editable-p)
                        (allows-cut-copy-paste nil allows-cut-copy-paste-p)
                      &allow-other-keys)
  (when (next-method-p) (call-next-method))
  (when image-p [self setImage: image])
  (when image-frame-style-p [self setImageFrameStyle: (ns-image-frame-style image-frame-style)])
  (when image-alignment-p [self setImageAlignment: (ns-image-alignment image-alignment)])
  (when image-scaling-p [self setImageScaling: (ns-image-scaling image-scaling)])
  (when animates-p [self setAnimates: (nsbool animates)])
  (when editable-p [self setEditable: (nsbool editable)])
  (when allows-cut-copy-paste-p [self setAllowsCutCopyPaste: (nsbool allows-cut-copy-paste)])
  self)


(defmethod configure ((self ns:ns-text-field)
                      &key
                        (editable nil editable-p)
                        (selectable nil selectable-p)
                        (allows-editing-text-attributes nil allows-editing-text-attributes-p)
                        (imports-graphics nil imports-graphics-p)
                        (text-color nil text-color-p)
                        (background-color nil background-color-p)
                        (draws-background nil draws-background-p)
                        (preferred-max-layout-width nil preferred-max-layout-width-p)
                        (bezeled nil bezeled-p)
                        (bezel-style nil bezel-style-p)
                        (bordered nil bordered-p)
                        (delegate nil delegate-p)
                      &allow-other-keys)
  (declare (ignorable
            preferred-max-layout-width  
            preferred-max-layout-width-p))
  (when (next-method-p) (call-next-method))
  (when editable-p [self setEditable: (nsbool editable)])
  (when selectable-p [self setSelectable: (nsbool selectable)])
  (when allows-editing-text-attributes-p [self setAllowsEditingTextAttributes: (nsbool allows-editing-text-attributes)])
  (when imports-graphics-p [self setImportsGraphics: (nsbool imports-graphics)])
  (when text-color-p [self setTextColor: text-color])
  (when background-color-p [self setBackgroundColor: background-color])
  (when draws-background-p [self setDrawsBackground: draws-background])
  #+cocoa-10.10 (when preferred-max-layout-width-p [self setPreferredMaxLayoutWidth: (cgfloat preferred-max-layout-width)])
  (when bezeled-p [self setBezeled: (nsbool bezeled)]) 
  (when bezel-style-p [self setBezelStyle: (ns-bezel-style bezel-style)])
  (when bordered-p [self setBordered: (nsbool bordered)])
  (when delegate-p [self setDelegate: delegate])
  self)
    
(defmethod configure ((self ns:ns-window)
                      &key
                        (title nil title-p)
                        (represented-filename nil represented-filename-p)
                        (represented-url nil represented-url-p)
                        (movable nil movable-p)
                        (movable-by-window-background nil movable-by-window-background-p)
                        (one-shot nil one-shot-p)
                        (style-mask nil style-mask-p)
                        (works-when-modal nil works-when-modal-p)
                        (alpha-value nil alpha-value-p)
                        (background-color nil background-color-p)
                        (color-space nil color-space-p)
                        (content-view nil content-view-p)
                        (can-hide nil can-hide-p)
                        (hides-on-deactivate nil hides-on-deactivate-p)
                        (collection-behavior nil collection-behavior-p)
                        (opaque nil opaque-p)
                        (has-shadow nil has-shadow-p)
                        (invalidate nil invalidate-p)
                        (delegate nil delegate-p)
                        (initial-first-responder nil initial-first-responder-p)
                        (prevents-application-termination-when-modal nil prevents-application-termination-when-modal-p)
                        (sharing-type nil sharing-type-p)
                        (can-become-visible-without-login nil can-become-visible-without-login-p)
                        (backing-type nil backing-type-p)
                        (preferred-backing-location nil preferred-backing-location-p)
                        (depth-limit nil depth-limit-p)
                        (dynamic-depth-limit nil dynamic-depth-limit-p)
                        (window-controller nil window-controller-p)
                        (aspect-ratio nil aspect-ratio-p)
                        (min-size nil min-size-p)
                        (max-size nil max-size-p)
                        (shows-resize-indicator nil shows-resize-indicator-p)
                        (resize-increments nil resize-increments-p)
                        (preserves-content-during-live-resize nil preserves-content-during-live-resize-p)
                        (content-min-size nil content-min-size-p)
                        (content-max-size nil content-max-size-p)
                        (content-resize-increments nil content-resize-increments-p)
                        (level nil level-p)
                        (frame-using-name nil frame-using-name-p)
                        (frame-autosave-name nil frame-autosave-name-p)
                        (frame-from-string nil frame-from-string-p)
                        (toolbar nil toolbar-p)
                        (default-button-cell nil default-button-cell-p)
                        (excluded-from-windows-menu nil excluded-from-windows-menu-p)
                        (shows-toolbar-button nil shows-toolbar-button-p)
                        (allows-tool-tips-when-application-is-inactive nil allows-tool-tips-when-application-is-inactive-p)
                        (accepts-mouse-moved-events nil accepts-mouse-moved-events-p)
                        (restorable nil restorable-p)
                        (restoration-class nil restoration-class-p)
                        (autodisplay nil autodisplay-p)
                        (allows-concurrent-view-drawing nil allows-concurrent-view-drawing-p)
                        (animation-behavior nil animation-behavior-p)
                        (miniwindow-image nil miniwindow-image-p)
                        (miniwindow-title nil miniwindow-title-p)
                        (anchor-attribute-for-orientation nil anchor-attribute-for-orientation-p)
                      &allow-other-keys)
  (declare (ignorable
            invalidate                        
            invalidate-p                      
            restorable                        
            restorable-p                      
            restoration-class                 
            restoration-class-p               
            animation-behavior                
            animation-behavior-p              
            anchor-attribute-for-orientation  
            anchor-attribute-for-orientation-p))
  (when (next-method-p) (call-next-method))
  (when title-p [self setTitle: (nsstring title)])
  (when represented-filename-p [self setRepresentedFilename: (nsstring represented-filename)])
  (when represented-url-p [self setRepresentedURL: (nsurl represented-url)])
  (when movable-p [self setMovable: (nsbool movable)])
  (when movable-by-window-background-p [self setMovableByWindowBackground: (nsbool movable-by-window-background)])
  (when one-shot-p [self setOneShot: (nsbool one-shot)])
  (when style-mask-p [self setStyleMask: (ns-window-style-mask style-mask)])
  (when works-when-modal-p [self setWorksWhenModal: (nsbool works-when-modal)])
  (when alpha-value-p [self setAlphaValue: (cgfloat alpha-value)])
  (when background-color-p [self setBackgroundColor: background-color])
  (when color-space-p [self setColorSpace: color-space])
  (when content-view-p [self setContentView: content-view])
  (when can-hide-p [self setCanHide: (nsbool can-hide)])
  (when hides-on-deactivate-p [self setHidesOnDeactivate: (nsbool hides-on-deactivate)])
  (when collection-behavior-p [self setCollectionBehavior: (ns-window-collection-behavior collection-behavior)])
  (when opaque-p [self setOpaque: (nsbool opaque)])
  (when has-shadow-p [self setHasShadow: (nsbool has-shadow)])
  #+cocoa-10.10 (when invalidate-p [self setInvalidate: (nsbool invalidate)])
  (when delegate-p [self setDelegate: delegate])
  (when initial-first-responder-p [self setInitialFirstResponder: initial-first-responder])
  (when prevents-application-termination-when-modal-p [self setPreventsApplicationTerminationWhenModal: (nsbool prevents-application-termination-when-modal)])
  (when sharing-type-p [self setSharingType: (ns-window-sharing-type sharing-type)])
  (when can-become-visible-without-login-p [self setCanBecomeVisibleWithoutLogin: (nsbool can-become-visible-without-login)])
  (when backing-type-p [self setBackingType: backing-type])
  (when preferred-backing-location-p [self setPreferredBackingLocation: preferred-backing-location])
  (when depth-limit-p [self setDepthLimit: depth-limit])
  (when dynamic-depth-limit-p [self setDynamicDepthLimit: dynamic-depth-limit])
  (when window-controller-p [self setWindowController: window-controller])
  (when aspect-ratio-p [self setAspectRatio: (cgfloat aspect-ratio)])
  (when min-size-p [self setMinSize: (nssize min-size)])
  (when max-size-p [self setMaxSize: (nssize max-size)])
  (when shows-resize-indicator-p [self setShowsResizeIndicator: shows-resize-indicator])
  (when resize-increments-p [self setResizeIncrements: resize-increments])
  (when preserves-content-during-live-resize-p [self setPreservesContentDuringLiveResize: preserves-content-during-live-resize])
  (when content-min-size-p [self setContentMinSize: (nssize content-min-size)])
  (when content-max-size-p [self setContentMaxSize: (nssize content-max-size)])
  (when content-resize-increments-p [self setContentResizeIncrements: content-resize-increments])
  (when level-p [self setLevel: level])
  (when frame-using-name-p [self setFrameUsingName: (nsstring frame-using-name)])
  (when frame-autosave-name-p [self setFrameAutosaveName: (nsstring frame-autosave-name)])
  (when frame-from-string-p [self setFrameFromString: (nsstring frame-from-string)])
  (when toolbar-p [self setToolbar: toolbar])
  (when default-button-cell-p [self setDefaultButtonCell: default-button-cell])
  (when excluded-from-windows-menu-p [self setExcludedFromWindowsMenu: (nsbool excluded-from-windows-menu)])
  (when shows-toolbar-button-p [self setShowsToolbarButton: (nsbool shows-toolbar-button)])
  (when allows-tool-tips-when-application-is-inactive-p [self setAllowsToolTipsWhenApplicationIsInactive: (nsbool allows-tool-tips-when-application-is-inactive)])
  (when accepts-mouse-moved-events-p [self setAcceptsMouseMovedEvents: (nsbool accepts-mouse-moved-events)])
  #+cocoa-10.10 (when restorable-p [self setRestorable: (nsbool restorable)])
  #+cocoa-10.10 (when restoration-class-p [self setRestorationClass: restoration-class])
  (when autodisplay-p [self setAutodisplay: (nsbool autodisplay)])
  (when allows-concurrent-view-drawing-p [self setAllowsConcurrentViewDrawing: (nsbool allows-concurrent-view-drawing)])
  #+cocoa-10.10 (when animation-behavior-p [self setAnimationBehavior: animation-behavior])
  (when miniwindow-image-p [self setMiniwindowImage: miniwindow-image])
  (when miniwindow-title-p [self setMiniwindowTitle: (nsstring miniwindow-title)])
  #+cocoa-10.10 (when anchor-attribute-for-orientation-p [self setAnchorAttributeForOrientation: anchor-attribute-for-orientation])
  self)





(defun make-view (frame &rest keys &key &allow-other-keys)
  (let ((view [[NSView alloc] initWithFrame:(nsrect frame)]))
    (when keys (apply (function configure) view keys))
    view))

(defun make-image-view (frame &rest keys &key &allow-other-keys)
  (let ((view [[NSImageView alloc] initWithFrame:(nsrect frame)]))
    (when keys (apply (function configure) view keys))
    view))

(defun make-text-field (frame &rest keys &key &allow-other-keys) 
  (let ((view [[NSTextField alloc] initWithFrame:(nsrect frame)]))
    (when keys (apply (function configure) view keys))
    view))

(defun make-button (frame &rest keys &key &allow-other-keys) 
  (let ((view [[NSButton alloc] initWithFrame:(nsrect frame)]))
    (when keys (apply (function configure) view keys))
    view))

(defun make-window (frame style-mask backing defer &rest keys &key &allow-other-keys)
  (let ((window [[NSWindow alloc] initWithContentRect: (nsrect frame)
                 styleMask: (ns-window-style-mask style-mask)
                 backing: (ns-backing-store-type backing)
                 defer: (nsbool defer)]))
    (apply (function configure) window keys)
    window))



(defun ns-key-equivalent-modifier-mask (keywords)
  (loop
    :with m = 0
    :for k :in keywords
    :do (setf m (logior m (ecase k
                            ((:shift) #$NSShiftKeyMask)
                            ((:option :alternate) #$NSAlternateKeyMask)
                            ((:command) #$NSCommandKeyMask)
                            ((:control) #$NSControlKeyMask))))
    :finally (return m)))

(defmacro item (title &optional selector key-equivalent)
  (cond ((null    key-equivalent))
        ((stringp key-equivalent))
        ((listp   key-equivalent)
         (assert (= 1 (count-if (function stringp) key-equivalent))
                 (key-equivalent)
                 "Invalid key-equivalent: ~S~%Must be a string or a list containing a string and modifier keywords."
                 key-equivalent)
         (assert (every (lambda (k) (or (stringp k)
                                        (member k '(:shift :option :alternate :command :control))))
                        key-equivalent)
                 (key-equivalent)
                 "Invalid key-equivalent: ~S~%Must be a string or a list containing a string and modifier keywords."
                 key-equivalent))
        (t (error "Invalid key-equivalent: ~S~%Must be a string or a list containing a string and modifier keywords."
                  key-equivalent)))
  (let ((item `(let ((title  (objcl:objc-string ,title))
                     (action ,(if (null selector)
                                  'oclo:*null*
                                  `(oclo:@selector ,selector)))
                     (key-equivalent ,(cond
                                        ((null key-equivalent)    `(objcl:objc-string ""))
                                        ((stringp key-equivalent) `(objcl:objc-string ,key-equivalent))
                                        ((listp key-equivalent)   `(objcl:objc-string ,(find-if (function stringp) key-equivalent))))))
                 [[[NSMenuItem alloc] initWithTitle: title action: action keyEquivalent: key-equivalent] autorelease])))
    (if (consp key-equivalent)
        `(let ((item ,item)
               (mask ,(ns-key-equivalent-modifier-mask (remove-if-not (function keywordp) key-equivalent))))
           [item setKeyEquivalentModifierMask: mask]
           item)
        item)))

(defmacro menu (title &body items)
  `(let* ((menu-title ,(if title
                      `(objcl:objc-string ,title)
                      `(objcl:objc-string "")))
          (menu [[[NSMenu alloc] initWithTitle:menu-title] autorelease]))
     ,@(mapcar (lambda (item-form)
                 `(let ((item ,(cond
                                 ((and (or (symbolp item-form) (stringp item-form))
                                       (string= "-" item-form))
                                  `[NSMenuItem separatorItem])
                                 ((atom item-form)
                                  (error "Invalid item: ~S" item-form))
                                 (t (ecase (first item-form)
                                      (menu `(let ((item (item ,(second item-form)))
                                                   (menu ,item-form))
                                               [item setSubmenu:menu]
                                               item))
                                      (item item-form))))))
                    [menu addItem: item]))
               items)
     menu))



(defun dictionary (&rest key-values &key &allow-other-keys)
  (flet ((objclize (object)
           (typecase object
             (null       (objcl:objc-string "NO"))
             ((member t) (objcl:objc-string "YES"))
             (string     (objcl:objc-string object))
             (symbol     (objcl:objc-string (symbol-name object)))
             (t          object))))
   (loop
     :with dict = [NSMutableDictionary dictionaryWithCapacity: (truncate (length key-values) 2)]
     :for (key value) :on key-values :by (function cddr)
     :do (let ((key   (objclize key))
               (value (objclize value)))
           [dict setObject:value forKey:key])
     :finally (return dict))))


;;; THE END;;;
