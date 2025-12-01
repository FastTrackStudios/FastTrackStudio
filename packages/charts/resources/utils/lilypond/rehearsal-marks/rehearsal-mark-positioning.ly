% Rehearsal Mark Positioning for LilyPond
% This file contains functions for positioning and styling rehearsal marks
% with dynamic alignment to staff lines and left margins

% Include capsule utilities for styling
\include "../capsules/capsule-utils.ly"

% Function to calculate available left space for positioning
#(define (calculate-available-left-space layout)
   "Calculate the available left space for positioning marks"
   (let* ((left-margin (ly:output-def-lookup layout 'left-margin 10))
          (max-width (ly:output-def-lookup layout 'max-width 100)))
     (if (and (number? left-margin) (number? max-width))
         (- max-width left-margin)
         80))) 

% Function to calculate staff height for capsule sizing
#(define (calculate-staff-height layout)
   "Calculate the staff height for capsule sizing"
   (let* ((staff-space (ly:output-def-lookup layout 'staff-space 1.0))
          (line-thickness (ly:output-def-lookup layout 'line-thickness 0.1)))
     (if (and (number? staff-space) (number? line-thickness))
         (+ (* staff-space 4) line-thickness)
         4.0)))  

% Function to get staff position for alignment
#(define (get-staff-position grob)
   "Get the staff position for alignment calculations"
   (let* ((staff (ly:grob-parent grob Y))
          (staff-extent (if staff (ly:grob-extent staff staff Y) (cons 0 0)))
          (staff-top (cdr staff-extent))
          (staff-bottom (car staff-extent)))
     (cons staff-top staff-bottom)))

% Function to calculate Y offset for staff alignment
#(define (calculate-staff-alignment-offset grob)
   "Calculate the Y offset needed to align with staff line"
   (let* ((grob-extent (ly:grob-extent grob grob Y))
          (grob-top (cdr grob-extent))
          (staff-pos (get-staff-position grob))
          (staff-top (car staff-pos))
          (y-difference (- staff-top grob-top)))
     y-difference))

% Function to calculate dynamic Y-offset that adapts to staff position changes
#(define (dynamic-y-offset-callback grob)
   "Calculate Y-offset that dynamically adapts to staff position changes"
   (let* ((staff (ly:grob-parent grob Y))
          (staff-extent (if staff (ly:grob-extent staff staff Y) (cons 0 0)))
          (staff-top (cdr staff-extent))
          (staff-bottom (car staff-extent))
          (staff-center (/ (+ staff-top staff-bottom) 2))
          (grob-extent (ly:grob-extent grob grob Y))
          (grob-center (/ (+ (cdr grob-extent) (car grob-extent)) 2))
          ;; Calculate Y offset to align with staff center
          (y-offset (- staff-center grob-center)))
     y-offset))

% Function to calculate left margin offset
#(define (calculate-left-margin-offset layout)
   "Calculate the left margin offset for positioning"
   (let* ((left-margin (ly:output-def-lookup layout 'left-margin 10))
          (left-offset (if (number? left-margin)
                           (- left-margin)  ; Just use negative left margin
                           -10)))  ; Default offset if lookup fails
     left-offset))

% Main callback function for positioning rehearsal marks
#(define-public (position-rehearsal-mark-callback grob)
   "Main callback function for positioning rehearsal marks with dynamic alignment"
   (let* ((break-dir (ly:item-break-dir grob))
          (is-line-start (is-grob-at-line-start? grob))
          (original-text (ly:grob-property grob 'text))
          (layout (ly:grob-layout grob))
          (props (ly:grob-alist-chain grob (ly:output-def-lookup layout 'text-font-defaults)))
          (thickness (* (ly:output-def-lookup layout 'line-thickness) 1))
          (max-width (calculate-available-left-space layout))
          (staff-height (calculate-staff-height layout))
          (capsule-stencil (capsule-stencil-with-optimal-text layout props original-text thickness max-width staff-height))
          (left-offset (calculate-left-margin-offset layout)))
     
     ;; (ly:message "POSITIONING DEBUG: text=~a" original-text)
     ;; (ly:message "  is-line-start=~a" is-line-start)
     ;; (ly:message "  left-offset=~a" left-offset)
     
     (if is-line-start
         (begin
           ;; First-in-line marks: red color and left margin positioning
           (let* ((red-stencil (ly:stencil-in-color capsule-stencil 1.0 0.0 0.0)))
             (ly:grob-set-property! grob 'stencil red-stencil)
             ;; Use dynamic Y-offset callback that adapts to staff position
             (ly:grob-set-property! grob 'Y-offset dynamic-y-offset-callback)
             ;; Use fixed X-offset for left margin positioning
             (ly:grob-set-property! grob 'X-offset left-offset)))
         (begin
           ;; Normal marks: black color and normal positioning
           (ly:grob-set-property! grob 'stencil capsule-stencil)
           ;; Use dynamic Y-offset callback for consistent alignment
           (ly:grob-set-property! grob 'Y-offset dynamic-y-offset-callback)))
     
     grob))
