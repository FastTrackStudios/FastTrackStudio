% Testing utilities for LilyPond charting workflow
% This file contains test functions for debugging and development

% Test the key change line start callback function
% This will help us see which measures are at line starts
#(define-public (test-key-change-callback)
   "Test function to check line start positions for key changes"
   (ly:message "=== TESTING KEY CHANGE LINE START CALLBACK ===")
   (let ((test-measures '(59 67 73)))  ; The key change measures from our output
     (for-each 
      (lambda (measure)
        (let ((is-line-start (key-change-line-start-callback measure)))
          (ly:message "Measure ~a: is-line-start=~a" measure is-line-start)))
      test-measures))
   (ly:message "=== END TEST ==="))

% Test the new functions for handling key changes not at line starts
#(define-public (test-key-change-handling)
   "Test function to demonstrate key change handling for measures not at line starts"
   (ly:message "=== TESTING KEY CHANGE HANDLING ===")
   (let ((test-measures '(59 67 73)))  ; The key change measures from our output
     (for-each 
      (lambda (measure)
        (let ((next-break (handle-key-change-not-at-line-start measure)))
          (if next-break
              (ly:message "Measure ~a: next break for coloring = ~a" measure next-break)
              (ly:message "Measure ~a: no special handling needed" measure))))
      test-measures))
   
   ;; Test the function that gets all coloring measures
   (let ((coloring-measures (get-key-signature-coloring-measures '(59 67 73))))
     (ly:message "All measures that need key signature coloring: ~a" coloring-measures)
     
     ;; Test creating override events
     (let ((override-events (create-key-signature-override-events coloring-measures)))
       (ly:message "Created ~a override events for key signature coloring" (length override-events))))
   
   (ly:message "=== END KEY CHANGE HANDLING TEST ==="))

% Test function to show how to use the new music functions
#(define-public (test-music-functions)
   "Test function to demonstrate the new music functions for key signature coloring"
   (ly:message "=== TESTING MUSIC FUNCTIONS ===")
   (ly:message "Available music functions:")
   (ly:message "  - colorKeySignatureAtMeasure <measure-number>")
   (ly:message "  - colorKeySignatureAtLineBegin")
   (ly:message "  - colorKeySignatureAtLineEnd")
   (ly:message "Example usage:")
   (ly:message "  \\colorKeySignatureAtMeasure #75")
   (ly:message "  \\colorKeySignatureAtLineBegin")
   (ly:message "  \\colorKeySignatureAtLineEnd")
   (ly:message "=== END MUSIC FUNCTIONS TEST ==="))

% Function to run all tests
#(define-public (run-all-tests)
   "Run all available tests"
   (ly:message "=== RUNNING ALL TESTS ===")
   (test-key-change-callback)
   (test-key-change-handling)
   (test-music-functions)
   (ly:message "=== ALL TESTS COMPLETE ==="))
