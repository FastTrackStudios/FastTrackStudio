% Measure Position Detection Utilities for LilyPond
% Dynamic functions to detect line breaks and measure positions using LilyPond's internal grob system

% This file contains only measure position detection functions
% For rehearsal mark positioning and styling, see rehearsal-mark-positioning.ly

% Dynamic scheme function to detect if a grob is at the start of a line
% This uses LilyPond's internal grob system to determine line breaks
#(define-public (is-grob-at-line-start? grob)
   "Check if a grob is at the start of its line using ly:item-break-dir"
   (and (ly:item? grob)
        (= 1 (ly:item-break-dir grob))))

% Dynamic scheme function to detect if a grob is at the end of a line  
#(define-public (is-grob-at-line-end? grob)
   "Check if a grob is at the end of its line using ly:item-break-dir"
   (and (ly:item? grob)
        (= -1 (ly:item-break-dir grob))))

% Dynamic scheme function to detect if a grob is in the middle of a line
#(define-public (is-grob-in-line-middle? grob)
   "Check if a grob is in the middle of its line using ly:item-break-dir"
   (and (ly:item? grob)
        (= 0 (ly:item-break-dir grob))))

% Function to get the system that a grob belongs to
#(define-public (get-grob-system grob)
   "Get the system that a grob belongs to"
   (ly:grob-system grob))

% Function to check if two grobs are on the same system
#(define-public (grobs-on-same-system? grob1 grob2)
   "Check if two grobs are on the same system"
   (let* ((system1 (get-grob-system grob1))
          (system2 (get-grob-system grob2)))
     (and system1 system2 (eq? system1 system2))))

% Function to analyze measure positioning in the score
#(define-public (analyze-measure-positions)
   "Analyze the actual measure positions and line breaks in the score"
   (ly:message "=== MEASURE POSITION ANALYSIS ===")
   ;; This would need to be called after the score is processed
   ;; For now, let's create a test that shows how it would work
   (ly:message "Measure analysis function defined - ready to use with actual grobs"))

% Function to test our dynamic functions with a specific grob
#(define-public (test-grob-line-position grob)
   "Test our dynamic functions with a specific grob"
   (if grob
       (let* ((is-line-start (is-grob-at-line-start? grob))
              (is-line-end (is-grob-at-line-end? grob))
              (is-line-middle (is-grob-in-line-middle? grob))
              (system (get-grob-system grob))
              (break-dir (ly:item-break-dir grob)))
         (ly:message "GROB TEST: break-dir=~a, line-start=~a, line-end=~a, line-middle=~a, system=~a"
                     break-dir is-line-start is-line-end is-line-middle system)
         (list is-line-start is-line-end is-line-middle))
       (ly:message "GROB TEST: No grob provided")))

% Function to test line break detection using after-line-breaking callback
#(define-public (test-line-break-detection grob)
   "Test our dynamic line break detection using after-line-breaking callback"
   (let* ((break-dir (ly:item-break-dir grob))
          (is-line-start (is-grob-at-line-start? grob))
          (is-line-end (is-grob-at-line-end? grob))
          (is-line-middle (is-grob-in-line-middle? grob))
          (system (get-grob-system grob)))
     (ly:message "LINE BREAK TEST: break-dir=~a, line-start=~a, line-end=~a, line-middle=~a, system=~a"
                 break-dir is-line-start is-line-end is-line-middle system)
     grob))

% Note: The positioning callback function has been moved to rehearsal-mark-positioning.ly
% This file now contains only the measure position detection functions

% Custom rehearsal mark formatter that makes marks red if they're first in line
#(define-public (red-first-in-line-mark-formatter mark context)
   "Formatter that makes rehearsal marks red if they're the first in their line"
   (ly:message "RED FORMATTER: mark=~a, context=~a" mark context)
   (let* ((grob (ly:context-property context 'currentRehearsalMark))
          (is-line-start (if grob (is-grob-at-line-start? grob) #f)))
     (ly:message "RED FORMATTER: grob=~a, is-line-start=~a" grob is-line-start)
     (if is-line-start
         (begin
           (ly:message "RED FORMATTER: Making mark RED!")
           ;; If mark is already a markup, wrap it with color
           (if (markup? mark)
               (markup #:with-color "red" mark)
               (markup #:with-color "red" mark)))
         (begin
           (ly:message "RED FORMATTER: Keeping mark normal color")
           mark))))

% Custom rehearsal mark formatter that tests our dynamic functions
#(define-public (test-dynamic-mark-formatter mark context)
   "Test formatter that uses our dynamic line break detection"
   (ly:message "Formatter called with mark: ~a" mark)
   (let* ((grob (ly:context-property context 'currentRehearsalMark))
          (is-line-start (if grob (is-grob-at-line-start? grob) #f))
          (is-line-end (if grob (is-grob-at-line-end? grob) #f))
          (is-line-middle (if grob (is-grob-in-line-middle? grob) #f))
          (system (if grob (get-grob-system grob) #f))
          (break-dir (if grob (ly:item-break-dir grob) #f)))
     (ly:message "Mark: ~a, grob: ~a, break-dir: ~a, line-start: ~a, line-end: ~a, line-middle: ~a, system: ~a" 
                 mark grob break-dir is-line-start is-line-end is-line-middle system)
     mark))
