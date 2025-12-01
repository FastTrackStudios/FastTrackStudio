% Auto-four-measure-break utilities for generating line breaks every 4 measures within sections

#(define-public (analyze-section-measure-counts marks-music)
   "Analyze marks music and return a list of measure counts for each section"
   (let ((section-measure-counts '())
         (current-section-measures 0))
     
     (define (process-element element)
       (cond
        ((eq? (ly:music-property element 'name) 'AdHocMarkEvent)
         ;; Add current section measure count to list
         (set! section-measure-counts (append section-measure-counts (list current-section-measures)))
         ;; Reset for new section
         (set! current-section-measures 0))
        
        ((eq? (ly:music-property element 'name) 'SkipEvent)
         (let* ((duration (ly:music-property element 'duration))
                (measures (if duration (ly:duration-scale duration) 1)))
           (set! current-section-measures (+ current-section-measures measures))))))
     
     (if (eq? (ly:music-property marks-music 'name) 'SequentialMusic)
         (for-each process-element (ly:music-property marks-music 'elements))
         (process-element marks-music))
     
     ;; Add final section measure count
     (set! section-measure-counts (append section-measure-counts (list current-section-measures)))
     
     section-measure-counts))

#(define-public (generate-four-measure-breaks section-measure-counts)
   "Generate break positions every 4 measures within each section"
   (let ((break-positions '())
         (current-position 0))
     
     (for-each (lambda (section-measures)
                 ;; Generate breaks every 4 measures within this section
                 (let ((section-breaks '()))
                   (do ((measure 4 (+ measure 4)))
                       ((> measure section-measures))
                     (set! section-breaks (append section-breaks (list (+ current-position measure)))))
                   ;; Add section breaks to main list
                   (set! break-positions (append break-positions section-breaks))
                   ;; Update current position for next section
                   (set! current-position (+ current-position section-measures))))
               section-measure-counts)
     
     break-positions))

#(define-public (analyze-marks-for-four-measure-breaks marks-music)
   "Analyze marks music and return break positions for 4-measure breaks within sections"
   (let ((section-measure-counts (analyze-section-measure-counts marks-music)))
     ;; (ly:message "Section measure counts: ~a" section-measure-counts)
     (generate-four-measure-breaks section-measure-counts)))

printSectionMeasureCounts = #(define-music-function (marks) (ly:music?)
   "Print measure counts for each section"
   (let ((counts (analyze-section-measure-counts marks)))
     (ly:message "Section measure counts: ~a" counts))
   marks)

autoFourMeasureBreaks = #(define-music-function (marks) (ly:music?)
   "Generate automatic line breaks every 4 measures within each section"
   (let ((break-positions (analyze-marks-for-four-measure-breaks marks)))
     ;; (ly:message "Four-measure break positions: ~a" break-positions)
     ;; Generate breaks based on positions
     (let ((result '()))
       (for-each (lambda (pos)
                   ;; Add break with force permission
                   (set! result (append result (list (make-music 'LineBreakEvent 'break-permission 'force)))))
                 break-positions)
       ;; Create the result as sequential music
       (make-music 'SequentialMusic 'elements result))))

% Combined function that analyzes marks and generates both section and four-measure breaks in one pass
autoSectionAndFourMeasureBreaks = #(define-music-function (marks) (ly:music?)
   "Generate both section breaks and 4-measure breaks within sections in one pass"
   (let ((all-break-positions '())
         (current-position 0)
         (current-section-measures 0))
     
     (define (process-element element)
       (cond
        ((eq? (ly:music-property element 'name) 'AdHocMarkEvent)
         ;; Add section break at current position
         (set! all-break-positions (append all-break-positions (list current-position)))
         ;; Generate four-measure breaks for the current section
         (let ((section-four-measure-breaks '()))
           (do ((measure 4 (+ measure 4)))
               ((> measure current-section-measures))
             (set! section-four-measure-breaks (append section-four-measure-breaks (list (+ current-position measure)))))
           (set! all-break-positions (append all-break-positions section-four-measure-breaks)))
         ;; Reset for new section
         (set! current-position (+ current-position current-section-measures))
         (set! current-section-measures 0))
        
        ((eq? (ly:music-property element 'name) 'SkipEvent)
         (let* ((duration (ly:music-property element 'duration))
                (measures (if duration (ly:duration-scale duration) 1)))
           (set! current-section-measures (+ current-section-measures measures))))))
     
     ;; Process all elements
     (if (eq? (ly:music-property marks 'name) 'SequentialMusic)
         (for-each process-element (ly:music-property marks 'elements))
         (process-element marks))
     
     ;; Add final section break and four-measure breaks
     (set! all-break-positions (append all-break-positions (list current-position)))
     (let ((final-section-four-measure-breaks '()))
       (do ((measure 4 (+ measure 4)))
           ((> measure current-section-measures))
         (set! final-section-four-measure-breaks (append final-section-four-measure-breaks (list (+ current-position measure)))))
       (set! all-break-positions (append all-break-positions final-section-four-measure-breaks)))
     
     ;; Remove duplicates and sort
     (set! all-break-positions (sort (delete-duplicates all-break-positions) <))
     ;; (ly:message "Combined break positions: ~a" all-break-positions)
     
     ;; Generate breaks using the same approach as autoSectionBreaks
     (let ((result '())
           (previous-pos 0))
       (for-each (lambda (pos)
                   ;; Add skip for the measures between previous position and current position
                   (let ((measures-to-skip (- pos previous-pos)))
                     (if (> measures-to-skip 0)
                         (set! result (append result (list (make-music 'SkipEvent 'duration (ly:make-duration 0 0 measures-to-skip)))))))
                   ;; Add break with force permission
                   (set! result (append result (list (make-music 'LineBreakEvent 'break-permission 'force))))
                   ;; Update previous position
                   (set! previous-pos pos))
                 all-break-positions)
       ;; Create the result as sequential music
       (make-music 'SequentialMusic 'elements result))))
