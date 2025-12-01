% Capsule Markers for Rehearsal Marks
% Creates capsule markers with different styling for line-start vs middle marks

% Function to create a capsule marker only for marks at the beginning of a line
#(define-public (capsule-marker grob)
   "Create a capsule marker with red text only for marks at the beginning of a line"
   (let* ((original-text (ly:grob-property grob 'text))
          ;; Check if this mark is at the beginning of a line
          (is-line-start (is-grob-at-line-start? grob)))
     ;; Print the text being processed
     ;; (ly:message "Creating capsule marker for: ~a (line-start: ~a)" original-text is-line-start)
     ;; Apply different capsule styling based on line position
     (if is-line-start
         ;; For marks at the beginning of a line: use full-width capsule
         (let* ((capsule-stencil (make-rehearsal-mark-capsule grob)))
           ;; Set the capsule stencil as the stencil property
           (ly:grob-set-property! grob 'stencil capsule-stencil)
           ;; Use extra-offset for positioning (applied after all other positioning)
           (ly:grob-set-property! grob 'extra-offset (cons -0.5 -4.4))  ; Move right 0.5 and down 4.4 staff spaces
           grob)
         ;; For marks in the middle of a line: use optimal-width capsule (no extra positioning)
         (let* ((capsule-stencil (make-rehearsal-mark-capsule-optimal grob)))
           ;; Set the capsule stencil as the stencil property
           (ly:grob-set-property! grob 'stencil capsule-stencil)
           ;; No extra-offset for middle marks (they stay in their normal position)
           grob))))
