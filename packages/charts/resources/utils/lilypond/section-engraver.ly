% Section Engraver for LilyPond
% Custom engraver that tracks section labels and detects first measures of lines
% Uses dynamic line break detection from measure-position-detection.ly

% Include capsule utilities for styling
\include "capsules/capsule-utils.ly"

% Global variable to store line break information from RehearsalMark grobs
#(define-public line-break-info (make-hash-table))

% Function to store line break information from RehearsalMark grobs
#(define-public (store-line-break-info grob)
   "Store line break information from RehearsalMark grobs"
   (let* ((break-dir (ly:item-break-dir grob))
          (is-line-start (is-grob-at-line-start? grob))
          (text (ly:grob-property grob 'text)))
     (ly:message "STORING LINE BREAK INFO: text=~a, break-dir=~a, is-line-start=~a" 
                 text break-dir is-line-start)
     (hash-set! line-break-info text (list break-dir is-line-start))
     grob))

% Helper function to create section label grobs
#(define-public (create-section-label-grob translator event text)
   "Create a section label grob with dynamic positioning"
   (let ((grob (ly:engraver-make-grob translator 'TextScript event)))
     (ly:grob-set-property! grob 'text (markup #:capsule text))
     (ly:grob-set-property! grob 'direction UP)
     (ly:grob-set-property! grob 'self-alignment-X LEFT)
     ;; Use after-line-breaking callback to get stored line position info and adjust positioning
     (ly:grob-set-property! grob 'after-line-breaking 
       (lambda (grob)
         (let* ((stored-info (hash-ref line-break-info text #f))
                (is-line-start (if stored-info (cadr stored-info) #f)))
           (ly:message "Section label after-line-breaking: text=~a, stored-info=~a, is-line-start=~a" 
                       text stored-info is-line-start)
           (if stored-info
               ;; We have the stored info, apply positioning
               (if is-line-start
                   (begin
                     (ly:message "Section label: Positioning at far right for first-in-line")
                     (ly:grob-set-property! grob 'extra-offset (cons -8 0)) ; Far right positioning
                     (ly:grob-set-property! grob 'color red)) ; Color red for first-in-line
                   (begin
                     (ly:message "Section label: Normal positioning")
                     (ly:grob-set-property! grob 'extra-offset (cons 0 0)))) ; Normal left margin
               ;; No stored info yet, try to detect directly from the grob
               (let* ((break-dir (ly:item-break-dir grob))
                      (direct-is-line-start (is-grob-at-line-start? grob)))
                 (ly:message "Section label: No stored info, using direct detection: break-dir=~a, is-line-start=~a" 
                             break-dir direct-is-line-start)
                 (if direct-is-line-start
                     (begin
                       (ly:message "Section label: Direct detection - Positioning at far right for first-in-line")
                       (ly:grob-set-property! grob 'extra-offset (cons -8 0)) ; Far right positioning
                       (ly:grob-set-property! grob 'color red)) ; Color red for first-in-line
                     (begin
                       (ly:message "Section label: Direct detection - Normal positioning")
                       (ly:grob-set-property! grob 'extra-offset (cons 0 0)))))) ; Normal left margin
           grob)))
     grob))

% Custom Scheme engraver for section labels with dynamic line break detection
#(define-public (create-section-label-engraver context)
   "Create a custom engraver for section labels with dynamic line break detection"
   (let ((current-measure 0)
         (section-labels '()))
       
       (make-translator
        ((start-translation-timestep translator)
         "Called at the start of each timestep"
         (set! current-measure (ly:context-property context 'currentBarNumber 0)))
        
        ((process-music translator)
         "Process music - called for each timestep"
         (set! current-measure (ly:context-property context 'currentBarNumber 0)))
        
        (listeners
         ((ad-hoc-mark-event translator event)
          "Listen to ad-hoc mark events and create section labels"
          (let* ((mark-text (ly:event-property event 'text #f))
                 (text (if mark-text 
                           (if (string? mark-text) mark-text (markup->string mark-text))
                           "MARK")))
            (ly:message "Section label engraver: measure ~a, text: ~s" 
                        current-measure text)
            ;; Create a custom grob for our section label using the helper function
            (let ((grob (create-section-label-grob translator event text)))
              (set! section-labels (cons grob section-labels)))))
         
         ((rehearsal-mark-event translator event)
          "Listen to rehearsal mark events and create section labels"
          (let* ((mark-label (ly:event-property event 'label #f))
                 (text (if mark-label (number->string mark-label) "MARK")))
            (ly:message "Section label engraver: measure ~a, label: ~s" 
                        current-measure text)
            ;; Create a custom grob for our section label using the helper function
            (let ((grob (create-section-label-grob translator event text)))
              (set! section-labels (cons grob section-labels)))))))))