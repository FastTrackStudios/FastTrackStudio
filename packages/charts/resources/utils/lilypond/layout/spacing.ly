% Layout and Spacing Utilities

% Function to calculate exact padding to fill line width
#(define-public (calculate-line-padding grob)
   "Calculate exact padding needed to fill line width"
   (let* ((layout (ly:grob-layout grob))
          (line-width (ly:output-def-lookup layout 'line-width))
          (left-margin (ly:output-def-lookup layout 'left-margin))
          (right-margin (ly:output-def-lookup layout 'right-margin))
          (available-width (- line-width left-margin right-margin))
          (current-extent (ly:grob-extent grob grob X))
          (current-width (interval-length current-extent))
          (needed-padding (- available-width current-width)))
     (max 0 needed-padding)))

% Function to force ragged behavior by preventing stretching
#(define-public (force-ragged-behavior grob)
   "Force ragged behavior by preventing stretching, like page-spacing.cc does"
   (let* ((layout (ly:grob-layout grob))
          (line-width (ly:output-def-lookup layout 'line-width))
          (left-margin (ly:output-def-lookup layout 'left-margin))
          (right-margin (ly:output-def-lookup layout 'right-margin))
          (available-width (- line-width left-margin right-margin))
          (system (ly:grob-system grob)))
     
     ;; Log the attempt
     (ly:message "Force ragged behavior:")
     (ly:message "  available-width: ~a" available-width)
     (ly:message "  system: ~a" (if (ly:grob? system) "found" "not found"))
     
     ;; Force ragged behavior by setting spacing properties to prevent stretching
     ;; This is equivalent to setting force to 0 in the spring system
     (if (ly:grob? system)
         (begin
           ;; Set the system's spacing properties to use natural spacing (force = 0)
           ;; This prevents the stretching that normally happens with ragged-right = ##f
           (ly:grob-set-property! system 'spacing-increment 1.0)
           (ly:grob-set-property! system 'shortest-duration-space 1.0)
           ;; Also try to set the system to use ragged behavior
           (ly:grob-set-property! system 'ragged-right #t)
           (ly:message "  Forced ragged behavior: natural spacing (force = 0)"))))
     
     grob)
