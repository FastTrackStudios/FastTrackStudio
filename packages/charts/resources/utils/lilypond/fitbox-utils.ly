% LilyPond utility functions for fitBox and text formatting
% This file contains reusable functions for creating auto-fitting boxes and text formatting

% Utility function for centering text
#(define (center-text text)
  (markup #:center-align text))

% Auto-fitting fitBox markup command with manual word wrapping
#(define-markup-command (fitBox layout props content) (markup?)
  "Auto-fitting fitBox markup command with manual word wrapping"
  (let* ((target-width 20)  ; Target width slightly larger than left margin
         (text (if (string? content) content (format #f "~a" content)))
         (text-length (string-length text))
         (scale-factor (if (string-contains text "\n")
                           (let* ((pos (string-index text #\newline))
                                  (first-line (substring text 0 pos))
                                  (second-line (substring text (+ pos 1)))
                                  (first-length (string-length first-line))
                                  (second-length (string-length second-line))
                                  (max-length (max first-length second-length)))
                             (cond
                               ((> max-length 8) 0.7)    ; Very long line - scale down more
                               ((> max-length 6) 0.7)    ; Long line - scale down moderately
                               ((> max-length 4) 0.7)    ; Medium line - scale down slightly
                               ((> max-length 3) 0.9)    ; Medium line - scale down slightly
                               (else 1.0)))              ; Short lines - no scaling
                           (cond
                             ((> text-length 8) 0.7)    ; Very long text - scale down more
                             ((> text-length 6) 0.7)    ; Long text - scale down moderately
                             ((> text-length 4) 0.7)    ; Medium text - scale down slightly
                             ((> text-length 3) 0.9)    ; Medium text - scale down slightly
                             (else 1.0)))))             ; Short text - no scaling
    (ly:message "AUTO-FIT: Text: ~s, Length: ~a, Scale: ~a" text text-length scale-factor)
    (interpret-markup layout props
      (if (string-contains text "\n")
          (let* ((pos (string-index text #\newline))
                 (first (substring text 0 pos))
                 (second (substring text (+ pos 1)))
                 (two-lines (markup #:vcenter (make-column-markup (list (markup #:center-align first) (markup #:center-align second))))))
            (markup #:with-color "red" #:box #:bold #:fontsize 3 #:pad-around 0.5 #:scale (cons scale-factor scale-factor) #:override #'(font-family . "SF Pro Display")
              two-lines))
          (markup #:with-color "red" #:box #:bold #:fontsize 3 #:pad-around 0.5 #:scale (cons scale-factor scale-factor) #:override #'(font-family . "SF Pro Display")
            content)))))

% Auto-fitting fitBox markup command with no scaling
#(define-markup-command (fitBoxNoScale layout props content) (markup?)
  "Auto-fitting fitBox markup command with no scaling"
  (let* ((target-width 20)
         (text (if (string? content) content (format #f "~a" content)))
         (text-length (string-length text))
         (scale-factor 1.0))  ; No scaling
    (ly:message "AUTO-FIT: Text: ~s, Length: ~a, Scale: ~a" text text-length scale-factor)
    (interpret-markup layout props
      (markup #:with-color "red" #:box #:bold #:fontsize 3 #:pad-around 0.5 #:scale (cons scale-factor scale-factor) #:override #'(font-family . "SF Pro Display")
        (if (string-contains text "\n")
            (let* ((pos (string-index text #\newline))
                   (first (substring text 0 pos))
                   (second (substring text (+ pos 1))))
              (markup #:vcenter (make-column-markup (list first second))))
            content)))))

% Simple red box (back to working version)
#(define (red-box text)
  (markup #:with-color "red" #:box #:bold #:fontsize 3 text))
