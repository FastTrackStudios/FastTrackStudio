% Thin Staff Lines Utility
% Creates very thin staff lines (0.05 staff-space units) using custom stencil

% Function to create thin staff lines
#(define-public (thin-staff-lines grob)
  (define (index-cell cell dir)
    (if (equal? dir RIGHT)
        (cdr cell)
        (car cell)))

  (define (index-set-cell! x dir val)
    (case dir
      ((-1) (set-car! x val))
      ((1) (set-cdr! x val))))

  (let* ((common (ly:grob-system grob))
         (span-points '(0 . 0))
         (thickness 0.05)  ; Very thin staff lines
         (width (ly:grob-property grob 'width))
         (line-positions (ly:grob-property grob 'line-positions))
         (staff-space (ly:grob-property grob 'staff-space 1))
         (line-stencil #f)
         (total-lines empty-stencil))

    ;; Calculate span points like the original example
    (for-each
     (lambda (dir)
       (if (and (= dir RIGHT)
                (number? width))
           (set-cdr! span-points width)
           (let* ((bound (ly:spanner-bound grob dir))
                  (bound-ext (ly:grob-extent bound bound X)))
             
             (index-set-cell! span-points dir
                              (ly:grob-relative-coordinate bound common X))
             (if (and (not (ly:item-break-dir bound))
                      (not (interval-empty? bound-ext)))
                 (index-set-cell! span-points dir 
                                  (+ (index-cell span-points dir)
                                     (index-cell bound-ext dir))))))
       (index-set-cell! span-points dir (- (index-cell span-points dir)
                                           (* dir thickness 0.5))))
     (list LEFT RIGHT))

    (set! span-points
          (coord-translate span-points
                           (- (ly:grob-relative-coordinate grob common X))))
    (set! line-stencil
          (make-line-stencil thickness (car span-points) 0 (cdr span-points) 0))

    (if (pair? line-positions)
        (for-each (lambda (position)
                    (set! total-lines
                          (ly:stencil-add
                           total-lines
                           (ly:stencil-translate-axis
                            line-stencil
                            (* position staff-space 0.5) Y))))
                  line-positions)       
        (let* ((line-count (ly:grob-property grob 'line-count 5))
               (height (* (1- line-count) (/ staff-space 2))))
          (do ((i 0 (1+ i)))                      
              ((= i line-count))
            (set! total-lines (ly:stencil-add
                               total-lines
                               (ly:stencil-translate-axis
                                line-stencil
                                (- height (* i staff-space)) Y))))))
    total-lines))
