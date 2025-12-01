% LilyPond utility functions for capsule markup commands
% This file contains reusable functions for creating rounded rectangle capsules

%%%% Capsule Definitions:
#(define-public (make-capsule-stencil lgth radius thickness fill)
  "Make an capsule from four Bézier curves and two lines, of radius @var{radius},
   length @var{lgth}, and thickness @var{thickness} with fill
   defined by @code{fill}."
    (let*
        ((top-offset .5)
         (side-offset .5)
         (r-max radius)
         (r-min (- r-max))
         (l-max (- lgth r-max)) 
         (l-min (- l-max))
         (command-list `(moveto ,lgth 0                    ; Start at bottom-right corner
                         lineto ,lgth ,(- r-max top-offset) ; Go up to top-right corner (inset down)
                         curveto ,lgth ,r-max ,(- lgth side-offset) ,r-max ,(- lgth side-offset) ,r-max ; Curve to top-right corner
                         lineto ,(+ (- lgth) side-offset) ,r-max ; Go left to top-left corner (inset right)
                         curveto ,(- lgth) ,r-max ,(- lgth) ,(- r-max top-offset) ,(- lgth) ,(- r-max top-offset) ; Curve to top-left corner
                         lineto ,(- lgth) ,(+ r-min top-offset) ; Go down to bottom-left corner (inset up)
                         curveto ,(- lgth) ,r-min ,(+ (- lgth) side-offset) ,r-min ,(+ (- lgth) side-offset) ,r-min ; Curve to bottom-left corner
                         lineto ,(- lgth side-offset) ,r-min ; Go right to bottom-right corner (inset left)
                         curveto ,lgth ,r-min ,lgth ,(+ r-min top-offset) ,lgth ,(+ r-min top-offset) ; Curve to bottom-right corner
                         lineto ,lgth 0                     ; Go up back to start
                         closepath)))                       ; Close the rectangle
      ;; after Harm:
      (make-path-stencil
       command-list
       thickness 1 1 fill)))

#(define-public (calculate-available-left-space layout)
  "Calculate the available space in the left margin for instrument names"
  (let* ((left-margin (ly:output-def-lookup layout 'left-margin))
         (small-padding 0.5)  ; Small padding from left edge
         (available-space (- left-margin small-padding)))
    available-space))

#(define-public (calculate-staff-height layout)
  "Calculate the height of a standard 5-line staff"
  (let* ((staff-space (ly:output-def-lookup layout 'staff-space))
         (staff-height (* staff-space 4)))  ; 5 lines = 4 spaces between lines
    staff-height))

#(define-public (calculate-available-text-space capsule-width capsule-height thickness)
  "Calculate the available space for text within a capsule"
  (let* ((text-width (- capsule-width thickness thickness))  ; Account for thickness on both sides
         (text-height (- capsule-height thickness thickness))  ; Account for thickness on top and bottom
         (text-padding 0.2)  ; Small internal padding
         (available-width (- text-width text-padding text-padding))
         (available-height (- text-height text-padding text-padding)))
    (cons available-width available-height)))

#(define-public (analyze-text-pattern text)
  "Analyze text to determine optimal formatting pattern"
  (let* ((text-str (if (string? text) text (symbol->string text)))
         (has-space (string-contains text-str " "))
         (has-newline (string-contains text-str "\n")))
    (cond
      ;; Has newlines - convert to proper multi-line markup
      (has-newline
       (ly:message "~s: Multi-line pattern detected - converting to column markup" text-str)
       (let* ((lines (string-split text-str #\newline))
              (line-markups (map (lambda (line) (markup #:center-align line)) lines))
              (multi-line-markup (markup #:vcenter (make-column-markup line-markups))))
         multi-line-markup))
      ;; No spaces - single word, keep as is
      ((not has-space)
       ;; (ly:message "~s: Single word pattern detected" text-str)
       text)
      ;; Has spaces - check for number patterns
      (else
       (let* ((space-pos (string-index text-str #\space))
              (first-part (substring text-str 0 space-pos))
              (second-part (substring text-str (+ space-pos 1)))
              (first-is-number (string->number first-part))
              (second-is-number (string->number second-part)))
         (cond
           ;; Section + number pattern (e.g., "CH 2") - keep on one line
           (second-is-number
            (ly:message "~s: Section + number pattern detected: ~a ~a (HAS NUMBER)" text-str first-part second-part)
            text)
           ;; Number + section pattern - keep on one line
           (first-is-number
            (ly:message "~s: Number + section pattern detected: ~a ~a (HAS NUMBER)" text-str first-part second-part)
            text)
           ;; Multiple words without numbers - add newline between words
           (else
            (let* ((space-pos (string-contains text-str " "))
                   (first-word (substring text-str 0 space-pos))
                   (second-word (substring text-str (+ space-pos 1)))
                   (multi-line-text (string-append first-word "\n" second-word)))
              ;; Recursively process the newline-containing text
              (analyze-text-pattern multi-line-text)))))))))

#(define-public (find-optimal-font-size layout props text available-width available-height)
  "Find the optimal font size for text to fit within the available space"
  (let loop ((font-size 12))
    (let* ((test-stencil (interpret-markup layout props (markup #:fontsize font-size text)))
           (text-extent-x (ly:stencil-extent test-stencil X))
           (text-extent-y (ly:stencil-extent test-stencil Y))
           (text-width (interval-length text-extent-x))
           (text-height (interval-length text-extent-y)))
      (if (and (<= text-width available-width) (<= text-height available-height))
          font-size
          (if (<= font-size 1)
              1
              (loop (- font-size 0.5)))))))

#(define-public (calculate-capsule-width stencil thickness x-padding y-padding)
  "Calculate the width of a capsule that would be created for the given stencil"
  (let* ((x-ext (ly:stencil-extent stencil X))
         (text-width (interval-length x-ext))
         (x-length (+ text-width x-padding thickness)))
    x-length))

#(define-public (calculate-capsule-width-with-max stencil thickness x-padding y-padding max-width)
  "Calculate the width of a capsule with maximum width constraint"
  (let* ((x-ext (ly:stencil-extent stencil X))
         (text-width (interval-length x-ext))
         (available-width (- max-width thickness))
         (desired-width (+ text-width x-padding thickness))
         (x-length (min desired-width available-width)))
    x-length))

#(define-public (capsule-stencil stencil thickness x-padding y-padding)
  "Add a capsule around @code{stencil}, padded by the padding pair,
   producing a var stencil."
  (let* ((x-ext (ly:stencil-extent stencil X))
         (y-ext (ly:stencil-extent stencil Y))
         (x-length (+ (interval-length x-ext) x-padding thickness))
         (y-length (+ (interval-length y-ext) y-padding thickness))
         (x-radius (* 0.5 x-length) )
         (y-radius (* 0.52 y-length) )
         (capsule (make-capsule-stencil x-radius y-radius thickness #f)))
    (ly:stencil-add
     stencil
     (ly:stencil-translate capsule
                           (cons
                            (interval-center x-ext)
                            (interval-center y-ext))))))

#(define-public (capsule-stencil-with-max-width stencil thickness x-padding y-padding max-width)
  "Add a capsule around @code{stencil} that always uses the full available width"
  (let* ((x-ext (ly:stencil-extent stencil X))
         (y-ext (ly:stencil-extent stencil Y))
         (text-width (interval-length x-ext))
         (available-width (- max-width thickness))
         (x-length available-width)  ; Always use full available width
         (y-length (+ (interval-length y-ext) y-padding thickness))
         (x-radius (* 0.5 x-length) )
         (y-radius (* 0.52 y-length) )
         (capsule (make-capsule-stencil x-radius y-radius thickness #f)))
    (ly:stencil-add
     stencil
     (ly:stencil-translate capsule
                           (cons
                            (interval-center x-ext)
                            (interval-center y-ext))))))

#(define-public (capsule-stencil-with-staff-dimensions stencil thickness x-padding y-padding max-width staff-height)
  "Add a capsule around @code{stencil} that uses full available width and staff height"
  (let* ((x-ext (ly:stencil-extent stencil X))
         (y-ext (ly:stencil-extent stencil Y))
         (text-width (interval-length x-ext))
         (available-width (- max-width thickness))
         (x-length available-width)  ; Always use full available width
         (y-length staff-height)     ; Use staff height
         (x-radius (* 0.5 x-length) )
         (y-radius (* 0.5 y-length) )  ; Use half of staff height for radius
         (capsule (make-capsule-stencil x-radius y-radius thickness #f)))
    (ly:stencil-add
     stencil
     (ly:stencil-translate capsule
                           (cons
                            (interval-center x-ext)
                            (interval-center y-ext))))))

#(define-public (calculate-multi-line-height-adjustment layout props text)
  "Calculate additional height needed for multi-line text beyond single line"
  (let* ((text-str (if (string? text) text (symbol->string text)))
         (has-newline (string-contains text-str "\n")))
    (if has-newline
        (let* ((lines (string-split text-str #\newline))
               (num-lines (length lines))
               (extra-lines (- num-lines 1))  ; Number of additional lines beyond the first
               (line-height 1.8)  ; Increased height per line for better spacing
               (padding 0.6)  ; Additional padding for multi-line text
               (additional-height (+ (* extra-lines line-height) padding)))
          additional-height)
        0)))

#(define-public (calculate-text-positioning layout props text thickness max-width staff-height)
  "Calculate positioning for text within capsule, ensuring first line stays in same position"
  (let* ((text-str (if (string? text) 
                       text 
                       (if (symbol? text) 
                           (symbol->string text)
                           "MULTI-LINE")))  ; If it's a markup object, assume multi-line
         (has-newline (string-contains text-str "\n"))
         (multi-line-adjustment (if (string? text)
                                   (calculate-multi-line-height-adjustment layout props text)
                                   (* staff-height 0.8)))  ; Assume multi-line if markup object
         (y-length (+ staff-height multi-line-adjustment))
         (y-offset 0))  ; No offset - let the alignment system handle positioning
    (cons y-length y-offset)))

#(define-public (capsule-stencil-with-optimal-text layout props text thickness max-width staff-height)
  "Create a capsule with optimally sized text that fits perfectly within the capsule"
  (let* ((available-width (- max-width thickness))
         (x-length available-width)  ; Always use full available width
         (analyzed-text (analyze-text-pattern text))
         (positioning (calculate-text-positioning layout props analyzed-text thickness max-width staff-height))
         (y-length (car positioning))
         (y-offset (cdr positioning))
         (text-space (calculate-available-text-space x-length y-length thickness))
         (available-text-width (car text-space))
         (available-text-height (cdr text-space))
         (optimal-font-size (find-optimal-font-size layout props analyzed-text available-text-width available-text-height))
         (scaled-stencil (interpret-markup layout props (markup #:fontsize optimal-font-size analyzed-text)))
         (x-ext (ly:stencil-extent scaled-stencil X))
         (y-ext (ly:stencil-extent scaled-stencil Y))
         (x-radius (* 0.5 x-length) )
         (y-radius (* 0.5 y-length) )
         (capsule (make-capsule-stencil x-radius y-radius thickness #f))
         (text-str (if (string? text) text (symbol->string text)))
         (has-newline (string-contains text-str "\n"))
         (is-markup-object (not (string? analyzed-text)))
         ;; Create the final stencil without offset
         (final-stencil (ly:stencil-add
                        scaled-stencil
                        (ly:stencil-translate capsule
                                              (cons
                                               (interval-center x-ext)
                                               (interval-center y-ext)))))
         )
    final-stencil))

#(define-markup-command (capsule layout props arg)
  (markup?)
  #:category graphic
  #:properties ((thickness 1)
                (font-size 0)
                (x-padding 2)
                (y-padding 1)
                (y-offset 0))
"
@cindex drawing capsule around text

Draw a capsule around @var{arg}. Use @code{thickness},
@code{x-padding}, @code{x-padding} and @code{font-size} properties to determine
line thickness and padding around the markup.

@lilypond[verbatim,quote]
\\markup {
  \\capsule {
    Hi
  }
}
@end lilypond"

  (let ((th (* (ly:output-def-lookup layout 'line-thickness)
               thickness))
        (max-width (calculate-available-left-space layout))
        (staff-height (calculate-staff-height layout))
        (offset y-offset))
    (ly:stencil-in-color (capsule-stencil-with-optimal-text layout props arg th max-width staff-height) 1.0 0.0 0.0)))

#(define-markup-command (capsule-width layout props arg)
  (markup?)
  #:category graphic
  #:properties ((thickness 1)
                (font-size 0)
                (x-padding 2)
                (y-padding 1))
"
@cindex display capsule width

Display the width that a capsule would have for the given text.
Useful for debugging and layout calculations.

@lilypond[verbatim,quote]
\\markup {
  \\capsule-width {
    INTRO
  }
}
@end lilypond"
  (let* ((th (* (ly:output-def-lookup layout 'line-thickness)
                thickness))
         (pad-x (* (magstep font-size) x-padding))
         (pad-y (* (magstep font-size) y-padding))
         (m (interpret-markup layout props arg))
         (max-width (calculate-available-left-space layout))
         (capsule-width (calculate-capsule-width-with-max m th pad-x pad-y max-width)))
    (interpret-markup layout props 
      (markup #:number (number->string (inexact->exact (round capsule-width)))))))

%%%% Capsule Definitions End %%%%%%%%%%
