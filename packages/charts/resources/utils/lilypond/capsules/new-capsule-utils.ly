% New capsule utilities designed for working with grob objects
% This file contains functions specifically for modifying existing grob objects
% like rehearsal marks, rather than creating standalone markup

%%%% Grob-based Capsule Functions:

#(define-public (get-grob-layout-info grob)
   "Get layout information from a grob"
   (let* ((layout (ly:grob-layout grob))
          (staff-space (ly:output-def-lookup layout 'staff-space))
          (line-thickness (ly:output-def-lookup layout 'line-thickness))
          (left-margin (ly:output-def-lookup layout 'left-margin)))
     (list layout staff-space line-thickness left-margin)))

#(define-public (get-grob-context-info grob)
   "Get context information from a grob"
   (let* ((context (ly:grob-parent grob X))
          (staff-symbol (ly:grob-object grob 'staff-symbol))
          (staff-height (if (not (null? staff-symbol))
                            (let* ((height-interval (ly:staff-symbol::height staff-symbol)))
                              (- (cdr height-interval) (car height-interval)))
                            4)))  ; Default staff height
     (list context staff-symbol staff-height)))

#(define-public (calculate-grob-available-width grob)
   "Calculate available width for a grob in its context"
   (let* ((layout-info (get-grob-layout-info grob))
          (left-margin (cadddr layout-info))  ; 4th element
          (page-padding 1.0))  ; More padding from the page edge
     (- left-margin page-padding)))

#(define-public (create-grob-text-stencil grob text color)
   "Create a text stencil for a grob with specified color"
   (grob-interpret-markup grob (markup #:with-color color text)))

#(define-public (create-grob-capsule-stencil grob text-stencil thickness x-padding y-padding max-width staff-height)
   "Create a capsule stencil for a grob using staff dimensions"
   (let* ((x-ext (ly:stencil-extent text-stencil X))
          (y-ext (ly:stencil-extent text-stencil Y))
          (available-width (- max-width thickness))
          (x-length available-width)  ; Use full available width
          (y-length staff-height)     ; Use staff height
          (x-radius (* 0.5 x-length))
          (y-radius (* 0.5 y-length))
          (capsule (make-capsule-stencil x-radius y-radius thickness #f))
          (red-capsule (stencil-with-color capsule "red")))
     (ly:stencil-add
      text-stencil
      (ly:stencil-translate red-capsule
                            (cons
                             (interval-center x-ext)
                             (interval-center y-ext))))))

#(define-public (create-grob-capsule-with-optimal-width grob text-stencil thickness x-padding y-padding max-width staff-height)
   "Create a capsule with optimal width for the text content"
   (let* ((x-ext (ly:stencil-extent text-stencil X))
          (y-ext (ly:stencil-extent text-stencil Y))
          (text-width (interval-length x-ext))
          (desired-width (+ text-width x-padding thickness))
          (available-width (- max-width thickness))
          (x-length (min desired-width available-width))  ; Use optimal width
          (y-length staff-height)     ; Use staff height
          (x-radius (* 0.5 x-length))
          (y-radius (* 0.5 y-length))
          (capsule (make-capsule-stencil x-radius y-radius thickness #f))
          (red-capsule (stencil-with-color capsule "red")))
     (ly:stencil-add
      text-stencil
      (ly:stencil-translate red-capsule
                            (cons
                             (interval-center x-ext)
                             (interval-center y-ext))))))

#(define-public (apply-capsule-to-grob grob text color thickness x-padding y-padding use-full-width)
   "Apply a capsule to a grob with specified parameters"
   (let* ((layout-info (get-grob-layout-info grob))
          (context-info (get-grob-context-info grob))
          (layout (car layout-info))
          (line-thickness (caddr layout-info))  ; 3rd element
          (staff-height (caddr context-info))   ; 3rd element
          (max-width (calculate-grob-available-width grob))
          (actual-thickness (* line-thickness thickness))
          ;; Create text stencil
          (text-stencil (create-grob-text-stencil grob text color))
          ;; Create capsule stencil
          (capsule-stencil (if use-full-width
                               (create-grob-capsule-stencil grob text-stencil actual-thickness x-padding y-padding max-width staff-height)
                               (create-grob-capsule-with-optimal-width grob text-stencil actual-thickness x-padding y-padding max-width staff-height))))
     capsule-stencil))

%%%% Convenience Functions for Common Use Cases:

#(define-public (make-rehearsal-mark-capsule grob)
   "Create a capsule for a rehearsal mark with red text and full width"
   (let* ((original-text (ly:grob-property grob 'text)))
     (apply-capsule-to-grob grob original-text "red" 1 0.2 0.2 #t)))

#(define-public (make-rehearsal-mark-capsule-optimal grob)
   "Create a capsule for a rehearsal mark with red text and optimal width"
   (let* ((original-text (ly:grob-property grob 'text)))
     (apply-capsule-to-grob grob original-text "red" 1 0.4 0.2 #f)))

%%%% New Capsule Utilities End %%%%%%%%%%
