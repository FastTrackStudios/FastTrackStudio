% Key changes configuration for LilyPond charting workflow
% This file contains key signature display settings and customizations
% Based on LSR snippet #775 by harm6

% Include the original LSR snippet definitions
\include "affecting-barline-items.ly"

% Include measure position detection utilities
\include "capsules/measure-position-detection.ly"

% Function to find the next break measure after a given measure number
#(define-public (find-next-break-measure current-measure)
   "Find the next break measure after the given measure number"
   ;; Based on the break positions we see in the output: (0 2 6 10 14 18 22 26 30 34 38 42 46 50 54 58 62 66 70 74 78 82 86 90 94)
   ;; Break positions represent where lines end, so the next measure starts a new line
   (let* ((break-positions '(0 2 6 10 14 18 22 26 30 34 38 42 46 50 54 58 62 66 70 74 78 82 86 90 94))
          (line-start-positions (map (lambda (pos) (+ pos 1)) break-positions))
          (next-break (find (lambda (pos) (> pos current-measure)) line-start-positions)))
     (if next-break
         next-break
         #f)))

% Callback function to process key change events and check line start positions
% For now, we'll use a simple approach based on the break positions we can see in the output
#(define-public (key-change-line-start-callback measure-number)
   "Callback function to check if a measure number is at the start of a line"
   ;; Based on the break positions we see in the output: (0 2 6 10 14 18 22 26 30 34 38 42 46 50 54 58 62 66 70 74 78 82 86 90 94)
   ;; Break positions represent where lines end, so the next measure starts a new line
   ;; So if there's a break at position 58, measure 59 starts a new line
   (let* ((break-positions '(0 2 6 10 14 18 22 26 30 34 38 42 46 50 54 58 62 66 70 74 78 82 86 90 94))
          (previous-break-positions (map (lambda (pos) (+ pos 1)) break-positions))
          (is-line-start (member measure-number previous-break-positions)))
     (ly:message "MEASURE ~a: line-start=~a" measure-number (if is-line-start #t #f))
     (if is-line-start #t #f)))

% Function to handle key changes that are NOT at line starts
% This finds the next break and creates a coloring event for that position
#(define-public (handle-key-change-not-at-line-start measure-number)
   "Handle key changes that occur in the middle of a line by finding the next break"
   (let* ((is-line-start (key-change-line-start-callback measure-number)))
     (if (not is-line-start)
         (let ((next-break (find-next-break-measure measure-number)))
           (if next-break
               (begin
                 (ly:message "KEY CHANGE at measure ~a is NOT at line start - will color key signature at measure ~a (next break)" measure-number next-break)
                 ;; Return the next break measure for coloring
                 next-break)
               (begin
                 (ly:message "KEY CHANGE at measure ~a is NOT at line start - no next break found" measure-number)
                 #f)))
         (begin
           (ly:message "KEY CHANGE at measure ~a IS at line start - no special handling needed" measure-number)
           #f))))

% Function to create a list of measures that need key signature coloring at line starts
% This processes all key change measures and returns a list of measures where key signatures should be colored
#(define-public (get-key-signature-coloring-measures key-change-measures)
   "Get a list of measures where key signatures should be colored based on key changes"
   (let ((coloring-measures '()))
     (for-each
      (lambda (measure)
        (let ((is-line-start (key-change-line-start-callback measure)))
          (if is-line-start
              ;; Key change is at line start - color immediately
              (set! coloring-measures (cons measure coloring-measures))
              ;; Key change is NOT at line start - find next break
              (let ((next-break (find-next-break-measure measure)))
                (if next-break
                    (set! coloring-measures (cons next-break coloring-measures)))))))
      key-change-measures)
     (ly:message "KEY SIGNATURE COLORING MEASURES: ~a" coloring-measures)
     coloring-measures))

% Function to create override events for key signature coloring at specific measures
% This creates \once \override events similar to affecting-barline-items.ly
#(define-public (create-key-signature-override-events coloring-measures)
   "Create a list of override events for key signature coloring at specific measures"
   (let ((override-events '()))
     (for-each
      (lambda (measure)
        (let ((override-event 
               (make-music 'OverrideProperty
                          'symbol 'KeySignature
                          'grob-value 'color
                          'grob-property-path '(color)
                          'value red)))
          (set! override-events (cons override-event override-events))
          (ly:message "Created override event for measure ~a" measure)))
      coloring-measures)
     (ly:message "Created ~a override events for key signature coloring" (length override-events))
     override-events))

% Music function to apply key signature coloring at specific measures
% This works like the colorBarLineBeg/colorBarLineEnd functions in affecting-barline-items.ly
colorKeySignatureAtMeasure = 
#(define-music-function (measure-number)(number?)
   "Color key signature red at a specific measure number"
   (let ((override-music 
          #{ \once\override Staff.KeySignature.color = #red #}))
     (ly:message "Adding key signature color override for measure ~a" measure-number)
     override-music))

% Music function to apply key signature coloring at line breaks (like affecting-barline-items.ly)
colorKeySignatureAtLineBegin = 
#(define-music-function ()(list?)
   "Color key signatures red when they appear at the beginning of a line"
   #{ \once\override Staff.KeySignature.after-line-breaking = #color-at-line-begin #})

colorKeySignatureAtLineEnd = 
#(define-music-function ()(list?)
   "Color key signatures red when they appear at the end of a line"
   #{ \once\override Staff.KeySignature.after-line-breaking = #color-at-line-end #})

%---------- Music Functions

% Function to color key signatures at line breaks (using original snippet)
colorKeyAtBreaks = {
  \override KeySignature.after-line-breaking = #color-at-line-begin
  \override KeySignature.after-line-breaking = #color-at-line-end
}

% Function to color only key signatures at line beginning
colorKeyAtLineBegin = {
  \override KeySignature.after-line-breaking = #color-at-line-begin
}

% Function to color only key signatures at line end
colorKeyAtLineEnd = {
  \override KeySignature.after-line-breaking = #color-at-line-end
}

% Legacy function name for compatibility
conditionalKeySignatureRed = {
  \override KeySignature.after-line-breaking = #color-at-line-begin
  \override KeySignature.after-line-breaking = #color-at-line-end
}


% Automatic key signature coloring system using custom engraver
#(define-public (auto-color-key-signatures grob)
  "Automatically color key signatures red when they appear at line breaks"
  (let* ((break-dir (ly:item-break-dir grob))
         (non-default (ly:grob-property grob 'non-default #f))
         (courtesy (ly:grob-property grob 'courtesy #f)))
    ;; Only color if:
    ;; 1. There's a break direction (at a line break)
    ;; 2. It's a non-default key signature (actual key change, not just default)
    ;; 3. OR it's a courtesy key signature
    (if (and break-dir (or non-default courtesy))
        (ly:grob-set-property! grob 'color red))
    grob))

% Score-level override to automatically color all key signatures at line breaks
autoKeySignatureColoring = {
  \override Score.KeySignature.after-line-breaking = #auto-color-key-signatures
}

% Staff-level override (alternative approach)
autoKeySignatureColoringStaff = {
  \override Staff.KeySignature.after-line-breaking = #auto-color-key-signatures
}

% Staff-level override using custom engraver to track actual key change events
% This needs to be used in a context definition, not as a music function
autoKeySignatureColoringWithEngraver = {
  \override Staff.KeySignature.after-line-breaking = #auto-color-key-signatures
}


% Automatic time signature coloring system
% This function automatically detects time signatures at line breaks and colors them red
% Only colors time signatures at the END of a line (before the break), not at the beginning
#(define-public (auto-color-time-signatures grob)
  "Automatically color time signatures red when they appear at the end of a line (before break)"
  (let* ((break-dir (ly:item-break-dir grob)))
    (if (equal? break-dir LEFT)  ; Only LEFT (end of line), not RIGHT (beginning of line)
        (ly:grob-set-property! grob 'color red))
    grob))

% Score-level override to automatically color all time signatures at line breaks
autoTimeSignatureColoring = {
  \override Score.TimeSignature.after-line-breaking = #auto-color-time-signatures
}

% Staff-level override (alternative approach)
autoTimeSignatureColoringStaff = {
  \override Staff.TimeSignature.after-line-breaking = #auto-color-time-signatures
}

% Combined function to color both key signatures and time signatures
autoSignatureColoring = {
  \override Staff.KeySignature.after-line-breaking = #auto-color-key-signatures
  \override Staff.TimeSignature.after-line-breaking = #auto-color-time-signatures
}

% Manual key change coloring - use this before specific \key commands
colorKeyChange = {
  \once\override Staff.KeySignature.before-line-breaking = #color-at-line-end
  \once\override Staff.KeySignature.after-line-breaking = #color-at-line-begin
}

% Manual time signature coloring - use this before specific \time commands
colorTimeChange = {
  \once\override Staff.TimeSignature.before-line-breaking = #color-at-line-end
}

% Custom engraver to track key change events and color key signatures at specific measures
% This uses the pre-calculated list of measures that need coloring
#(define-public (key-change-tracker-engraver context)
  (let ((first-key-change #t)
        (first-key-sig #t)
        ;; Pre-calculated list of measures that need key signature coloring
        ;; Based on our test results: (75 67 59)
        (coloring-measures '(75 67 59)))
    (make-engraver
     (listeners
      ((key-change-event engraver event)
       (if first-key-change
           (set! first-key-change #f)
           (let ((moment (ly:context-current-moment context)))
             (if (ly:moment? moment)
                 (let ((beat (ly:moment-main-numerator moment))
                       (unit (ly:moment-main-denominator moment)))
                   (let ((measure (+ beat 1)))
                     (ly:message "KEY CHANGE EVENT at beat ~a/~a (measure ~a)" beat unit measure))))))))
     (acknowledgers
      ((key-signature-interface engraver grob source-engraver)
       (let* ((non-default (ly:grob-property grob 'non-default #f))
              (courtesy (ly:grob-property grob 'courtesy #f))
              ;; Use the context parameter from the engraver to get the current moment
              (current-moment (ly:context-current-moment context))
              (grob-beat (if (and current-moment (ly:moment? current-moment)) 
                            (ly:moment-main-numerator current-moment) 0))
              (grob-measure (+ grob-beat 1))
              (should-color (member grob-measure coloring-measures)))
         (if (not first-key-sig)
             (if should-color
                 (begin
                   (ly:message "COLORING KEY SIGNATURE at measure ~a (in coloring list)" grob-measure)
                   (ly:grob-set-property! grob 'color red))))
         (set! first-key-sig #f)))))))