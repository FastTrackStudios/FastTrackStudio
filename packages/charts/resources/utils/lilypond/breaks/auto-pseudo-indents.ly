% Auto-pseudo-indent utilities for automatically applying pseudo-indents to lines with < 4 measures

% Constants for pseudo-indent values based on line length
#(define pseudo-indent-values
   '((1 . 70)   ; 1 measure = pseudo-indent 80
     (2 . 44)   ; 2 measures = pseudo-indent 44  
     (3 . 20)   ; 3 measures = pseudo-indent 20
     (default . 44))) 

% Helper function to get pseudo-indent value based on line length
#(define (get-pseudo-indent-value line-length)
   (let ((entry (assoc line-length pseudo-indent-values)))
     (if entry
         (cdr entry)
         (cdr (assoc 'default pseudo-indent-values)))))

% Helper function to create pseudo-indent music with a given value
#(define (create-pseudo-indent-music value)
   #{ \pseudoIndents 0 #value #})

% Function to get barline position information from current system
#(define (get-barline-position-info context)
   (let* ((current-barline (ly:context-property context 'currentBarLine))
          (system (if (ly:grob? current-barline)
                      (ly:grob-system current-barline)
                      #f)))
     (if (and (ly:grob? current-barline) system)
         (let* ((barline-x-extent (ly:grob-property current-barline 'X-extent))
                (barline-relative-x (ly:grob-relative-coordinate current-barline system X))
                (barline-left-x (+ barline-relative-x (car barline-x-extent)))
                (barline-right-x (+ barline-relative-x (cdr barline-x-extent))))
           (list barline-left-x barline-right-x barline-relative-x))
         #f)))

% Enhanced function to calculate pseudo-indent based on barline alignment
#(define (calculate-barline-aligned-indent line-length context target-alignment)
   (let* ((barline-info (get-barline-position-info context))
          (base-indent (get-pseudo-indent-value line-length)))
     
     (if barline-info
         (let* ((barline-left (list-ref barline-info 0))
                (barline-right (list-ref barline-info 1))
                (barline-center (/ (+ barline-left barline-right) 2)))
           
           ;; Calculate indent based on target alignment
           (cond
            ;; Align with left edge of barline
            ((eq? target-alignment 'left)
             (max 0 (- barline-left 5))) ; 5 staff-spaces before barline
            
            ;; Align with center of barline  
            ((eq? target-alignment 'center)
             (max 0 (- barline-center 10))) ; 10 staff-spaces before center
            
            ;; Align with right edge of barline
            ((eq? target-alignment 'right)
             (max 0 (- barline-right 15))) ; 15 staff-spaces before right edge
            
            ;; Default: use base indent
            (else base-indent)))
         base-indent)))

% Function to create barline-aligned pseudo-indent music
#(define (create-barline-aligned-pseudo-indent-music line-length context alignment)
   (create-pseudo-indent-music (calculate-barline-aligned-indent line-length context alignment)))

% Convenience functions for different barline alignments
#(define (create-left-aligned-pseudo-indent line-length context)
   (create-barline-aligned-pseudo-indent-music line-length context 'left))

#(define (create-center-aligned-pseudo-indent line-length context)
   (create-barline-aligned-pseudo-indent-music line-length context 'center))

#(define (create-right-aligned-pseudo-indent line-length context)
   (create-barline-aligned-pseudo-indent-music line-length context 'right))

% Music function to apply barline-aligned pseudo-indent
barlineAlignedPseudoIndent = #(define-music-function (line-length alignment) (number? symbol?)
   "Apply pseudo-indent aligned with barline position"
   (let ((context (ly:parser-lookup 'context)))
     (create-barline-aligned-pseudo-indent-music line-length context alignment)))

% Function to get absolute page coordinates by traversing grob hierarchy
#(define (get-absolute-page-coordinates grob)
   (let* ((system (ly:grob-system grob))
          (page (if (ly:grob? system)
                    (ly:grob-parent system Y)
                    #f))
          (grob-relative-to-system (if (ly:grob? system)
                                       (ly:grob-relative-coordinate grob system X)
                                       0))
          (system-relative-to-page (if (and (ly:grob? system) (ly:grob? page))
                                       (ly:grob-relative-coordinate system page X)
                                       0))
          (absolute-x (+ grob-relative-to-system system-relative-to-page)))
     absolute-x))





% Function to calculate pseudo-indent based on line width and measure count
% Uses decimal multipliers for cleaner calculations
#(define (calculate-line-width-based-indent line-width measure-count)
   (cond
    ((= measure-count 1) (* line-width 0.75))   ; 1 measure = 75% of line width (more indent for short line)
    ((= measure-count 2) (* line-width 0.50))   ; 2 measures = 50% of line width (medium indent)
    ((= measure-count 3) (* line-width 0.25))   ; 3 measures = 25% of line width (less indent for longer line)
    (else (* line-width 0.50))))               

% Function to get line width in staff-space units (LilyPond's internal units)
% This uses reasonable defaults based on typical LilyPond settings
#(define (get-line-width-in-staff-space)
   (let* ((line-width-mm 150)  ; typical line width in mm (A4 with margins)
          (staff-space-mm 1.75)  ; typical staff-space in mm (LilyPond default)
          (line-width-staff-space (/ line-width-mm staff-space-mm)))
     (ly:message "LINE WIDTH CALCULATION: ~a mm / ~a mm = ~a staff-space units" 
                 line-width-mm staff-space-mm line-width-staff-space)
     line-width-staff-space))

% Updated pseudo-indent calculation that uses line width as fallback
#(define (get-pseudo-indent-value measure-count)
   (let* ((line-width (get-line-width-in-staff-space))
          (calculated-indent (calculate-line-width-based-indent line-width measure-count)))
     (ly:message "PSEUDO-INDENT CALCULATION: ~a measures -> line-width=~a staff-space -> indent=~a staff-space" 
                 measure-count line-width calculated-indent)
     calculated-indent))

% Function to log barline positions with measure numbers and absolute page coordinates
#(define (log-barline-positions-after-layout grob)
   (let* ((grob-name (grob::name grob))
          (system (ly:grob-system grob))
          (layout (ly:grob-layout grob))
          (line-width-mm (ly:output-def-lookup layout 'line-width))
          (staff-space-mm (ly:output-def-lookup layout 'staff-space))
          (line-width-staff-space (/ line-width-mm staff-space-mm))
          (barline-x-extent (ly:grob-property grob 'X-extent))
          (barline-relative-x (if (ly:grob? system)
                                  (ly:grob-relative-coordinate grob system X)
                                  0))
          (barline-anchor (ly:grob-property grob 'break-align-anchor))
          (barline-glyph (ly:grob-property grob 'glyph))
          (absolute-page-x (get-absolute-page-coordinates grob))
          (barline-left-x (if (pair? barline-x-extent)
                              (+ barline-relative-x (car barline-x-extent))
                              barline-relative-x))
          (barline-right-x (if (pair? barline-x-extent)
                               (+ barline-relative-x (cdr barline-x-extent))
                               barline-relative-x))
          (barline-center-x (if (number? barline-anchor)
                                (+ barline-relative-x barline-anchor)
                                barline-relative-x))
          (absolute-left-x (+ absolute-page-x (if (pair? barline-x-extent)
                                                  (car barline-x-extent)
                                                  0)))
          (absolute-center-x (+ absolute-page-x (if (number? barline-anchor)
                                                    barline-anchor
                                                    0)))
          (absolute-right-x (+ absolute-page-x (if (pair? barline-x-extent)
                                                   (cdr barline-x-extent)
                                                   0))))
     
     ;; Only log BarLine grobs (not SystemStartBar)
     (if (eq? grob-name 'BarLine)
         (begin
           ;; Increment counter for each barline
           (set! barline-counter (+ barline-counter 1))
           (ly:message "=== BARLINE POSITION LOG (MEASURE + COORDINATES) ===")
           (ly:message "Measure ~a ends at X = ~a (center: ~a)" 
                       barline-counter absolute-left-x absolute-center-x)
           (ly:message "  Glyph: ~a, System-relative: ~a, Page-absolute: ~a" 
                       (if (string? barline-glyph) barline-glyph "none")
                       barline-relative-x absolute-page-x)
           (ly:message "  Layout info: line-width=~a mm, staff-space=~a mm, line-width=~a staff-space" 
                       line-width-mm staff-space-mm line-width-staff-space)
           (ly:message "=====================================================")))))

% Music function to enable barline position logging after layout
logBarlinePositionsAfterLayout = #(define-music-function () ()
   "Log barline positions after layout is complete"
   #{
     \applyContext
     #(lambda (context)
        (ly:context-pushpop-property context 'BarLine 'after-line-breaking 
                                     log-barline-positions-after-layout)
        (ly:context-pushpop-property context 'SpanBar 'after-line-breaking 
                                     log-barline-positions-after-layout)
        (ly:context-pushpop-property context 'SystemStartBar 'after-line-breaking 
                                     log-barline-positions-after-layout))
   #})


% Function to log barline positions to a file
#(define (log-barline-positions-to-file grob grob-origin context)
   (let* ((system (ly:grob-system grob))
          (measure-number (ly:grob-property grob 'measure-count))
          (barline-x-extent (ly:grob-property grob 'X-extent))
          (barline-relative-x (ly:grob-relative-coordinate grob system X))
          (barline-anchor (ly:grob-property grob 'break-align-anchor))
          (barline-glyph (ly:grob-property grob 'glyph))
          (barline-left-x (+ barline-relative-x (car barline-x-extent)))
          (barline-right-x (+ barline-relative-x (cdr barline-x-extent)))
          (barline-center-x (+ barline-relative-x barline-anchor))
          (log-file (open-output-file "barline-positions.log")))
     
     (display (format #f "Measure: ~a, Glyph: ~a, Left: ~a, Center: ~a, Right: ~a, Anchor: ~a\n"
                      (if measure-number measure-number "unknown")
                      (if (string? barline-glyph) barline-glyph "none")
                      barline-left-x barline-center-x barline-right-x barline-anchor)
              log-file)
     (close-output-port log-file)))

% Music function to log barline positions to file
logBarlinePositionsToFile = #(define-music-function () ()
   "Log the position of every barline to barline-positions.log file"
   #{
     \applyOutput Score.BarLine #log-barline-positions-to-file
   #})

% Enhanced function to get detailed barline position info
#(define (get-detailed-barline-position-info context)
   (let* ((current-barline (ly:context-property context 'currentBarLine))
          (system (if (ly:grob? current-barline)
                      (ly:grob-system current-barline)
                      #f)))
     (if (and (ly:grob? current-barline) system)
         (let* ((measure-number (ly:grob-property current-barline 'measure-count))
                (barline-x-extent (ly:grob-property current-barline 'X-extent))
                (barline-relative-x (ly:grob-relative-coordinate current-barline system X))
                (barline-anchor (ly:grob-property current-barline 'break-align-anchor))
                (barline-glyph (ly:grob-property current-barline 'glyph))
                (barline-left-x (+ barline-relative-x (car barline-x-extent)))
                (barline-right-x (+ barline-relative-x (cdr barline-x-extent)))
                (barline-center-x (+ barline-relative-x barline-anchor)))
           (list measure-number barline-glyph barline-left-x barline-center-x barline-right-x barline-anchor))
         #f)))

% Function to analyze barline positions and suggest pseudo-indent values
#(define (analyze-barline-positions grob grob-origin context)
   (let* ((system (ly:grob-system grob))
          (measure-number (ly:grob-property grob 'measure-count))
          (barline-x-extent (ly:grob-property grob 'X-extent))
          (barline-relative-x (ly:grob-relative-coordinate grob system X))
          (barline-anchor (ly:grob-property grob 'break-align-anchor))
          (barline-glyph (ly:grob-property grob 'glyph))
          (barline-left-x (+ barline-relative-x (car barline-x-extent)))
          (barline-right-x (+ barline-relative-x (cdr barline-x-extent)))
          (barline-center-x (+ barline-relative-x barline-anchor))
          (line-width (ly:output-def-lookup (ly:grob-layout grob) 'line-width))
          (indent (ly:output-def-lookup (ly:grob-layout grob) 'indent))
          (short-indent (ly:output-def-lookup (ly:grob-layout grob) 'short-indent)))
     
     ;; Calculate suggested pseudo-indent values for different line lengths
     (let* ((suggested-1-measure (max 0 (- barline-left-x 10)))
            (suggested-2-measure (max 0 (- barline-center-x 15)))
            (suggested-3-measure (max 0 (- barline-right-x 20))))
       
       (ly:message "=== BARLINE ANALYSIS ===")
       (ly:message "Measure ~a (~a):" (if measure-number measure-number "unknown") 
                   (if (string? barline-glyph) barline-glyph "none"))
       (ly:message "  Barline positions: Left=~a, Center=~a, Right=~a" 
                   barline-left-x barline-center-x barline-right-x)
       (ly:message "  Suggested pseudo-indents:")
       (ly:message "    1-measure line: ~a" suggested-1-measure)
       (ly:message "    2-measure line: ~a" suggested-2-measure)
       (ly:message "    3-measure line: ~a" suggested-3-measure)
       (ly:message "  Current constants: 1=~a, 2=~a, 3=~a"
                   (cdr (assoc 1 pseudo-indent-values))
                   (cdr (assoc 2 pseudo-indent-values))
                   (cdr (assoc 3 pseudo-indent-values)))
       (ly:message "========================"))))

% Music function to analyze barline positions
analyzeBarlinePositions = #(define-music-function () ()
   "Analyze barline positions and suggest optimal pseudo-indent values"
   #{
     \applyOutput Score.BarLine #analyze-barline-positions
   #})

% Function to analyze marks and identify lines that need pseudo-indents
getShortLinePositions = #(define-music-function (marks) (ly:music?)
   "Analyze marks and identify positions where pseudo-indents should be applied"
   (let ((all-break-positions '())
         (current-position 0)
         (current-section-measures 0)
         (short-line-positions '())
         (total-measures 0))
     
     ;; First, analyze the marks to get all break positions (same logic as autoSectionAndFourMeasureBreaks)
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
     
     ;; Now analyze which lines will be shorter than 4 measures
     ;; First, check lines between breaks - we apply pseudo-indents at the START of short lines
     (let ((previous-pos 0))
       (for-each (lambda (current-pos)
                   (let ((line-length (- current-pos previous-pos)))
                     ;; If line length is less than 4 measures, mark for pseudo-indent at the START of this line
                     (if (and (< line-length 4) (> current-pos 0))
                         (set! short-line-positions (append short-line-positions (list previous-pos)))))
                   (set! previous-pos current-pos))
                 all-break-positions))
     
     ;; Also check the final line from the last break to the end of the score
     ;; We need to get the total length of the marks to determine the final line length
     (define (count-total-measures element)
       (cond
        ((eq? (ly:music-property element 'name) 'SkipEvent)
         (let* ((duration (ly:music-property element 'duration))
                (measures (if duration (ly:duration-scale duration) 1)))
           (set! total-measures (+ total-measures measures))))))
     
     (if (eq? (ly:music-property marks 'name) 'SequentialMusic)
         (for-each count-total-measures (ly:music-property marks 'elements))
         (count-total-measures marks))
     
     ;; Check if the final line (from last break to end) is shorter than 4 measures
     (let ((last-break-pos (if (null? all-break-positions) 0 (list-ref all-break-positions (- (length all-break-positions) 1)))))
       (let ((final-line-length (- total-measures last-break-pos)))
         (if (< final-line-length 4)
             (set! short-line-positions (append short-line-positions (list last-break-pos))))))
     
     ;; Print the analysis results
     (ly:message "=== PSEUDO-INDENT ANALYSIS ===")
     (ly:message "All break positions: ~a" all-break-positions)
     (ly:message "Total measures in score: ~a" total-measures)
     (ly:message "Short line positions (need pseudo-indents): ~a" short-line-positions)
     (ly:message "Lines that need pseudo-indents (applied at start of line):")
     (for-each (lambda (pos)
                 (if (= pos (if (null? all-break-positions) 0 (list-ref all-break-positions (- (length all-break-positions) 1))))
                     (ly:message "  - Final line starting at measure ~a (from last break to end)" pos)
                     (ly:message "  - Line starting at measure ~a" pos)))
               short-line-positions)
     (ly:message "================================")
     
     marks))

% Functions to create pseudo-indent music with different values
createPseudoIndentLarge = #(define-music-function () ()
   "Create pseudo-indent with value 80 (for 1-measure lines)"
   (create-pseudo-indent-music (get-pseudo-indent-value 1)))

createPseudoIndentMedium = #(define-music-function () ()
   "Create pseudo-indent with value 44 (for 2-measure lines)"
   (create-pseudo-indent-music (get-pseudo-indent-value 2)))

createPseudoIndentSmall = #(define-music-function () ()
   "Create pseudo-indent with value 20 (for 3-measure lines)"
   (create-pseudo-indent-music (get-pseudo-indent-value 3)))

% Function to automatically generate pseudo-indents at all short line positions
autoPseudoIndents = #(define-music-function (marks) (ly:music?)
   "Automatically generate pseudo-indents at all short line positions"
   (let ((all-break-positions '())
         (current-position 0)
         (current-section-measures 0)
         (total-measures 0)
         (short-line-positions '())
         (result '()))
     
     ;; First, analyze the marks to get all break positions (same logic as before)
     (define (process-element element)
       (cond
        ((eq? (ly:music-property element 'name) 'AdHocMarkEvent)
         (set! all-break-positions (append all-break-positions (list current-position)))
         (let ((section-four-measure-breaks '()))
           (do ((measure 4 (+ measure 4)))
               ((> measure current-section-measures))
             (set! section-four-measure-breaks (append section-four-measure-breaks (list (+ current-position measure)))))
           (set! all-break-positions (append all-break-positions section-four-measure-breaks)))
         (set! current-position (+ current-position current-section-measures))
         (set! current-section-measures 0))
        
        ((eq? (ly:music-property element 'name) 'SkipEvent)
         (let* ((duration (ly:music-property element 'duration))
                (measures (if duration (ly:duration-scale duration) 1)))
           (set! current-section-measures (+ current-section-measures measures))))))
     
     (if (eq? (ly:music-property marks 'name) 'SequentialMusic)
         (for-each process-element (ly:music-property marks 'elements))
         (process-element marks))
     
     (set! all-break-positions (append all-break-positions (list current-position)))
     (let ((final-section-four-measure-breaks '()))
       (do ((measure 4 (+ measure 4)))
           ((> measure current-section-measures))
         (set! final-section-four-measure-breaks (append final-section-four-measure-breaks (list (+ current-position measure)))))
       (set! all-break-positions (append all-break-positions final-section-four-measure-breaks)))
     
     (set! all-break-positions (sort (delete-duplicates all-break-positions) <))
     
     ;; Count total measures
     (define (count-total-measures element)
       (cond
        ((eq? (ly:music-property element 'name) 'SkipEvent)
         (let* ((duration (ly:music-property element 'duration))
                (measures (if duration (ly:duration-scale duration) 1)))
           (set! total-measures (+ total-measures measures))))))
     
     (if (eq? (ly:music-property marks 'name) 'SequentialMusic)
         (for-each count-total-measures (ly:music-property marks 'elements))
         (count-total-measures marks))
     
     ;; Find short line positions - we need to apply pseudo-indents at the START of short lines
     (let ((previous-pos 0))
       (for-each (lambda (current-pos)
                   (let ((line-length (- current-pos previous-pos)))
                     ;; If line length is less than 4 measures, apply pseudo-indent at the START of this line
                     (if (and (< line-length 4) (> current-pos 0))
                         (set! short-line-positions (append short-line-positions (list previous-pos)))))
                   (set! previous-pos current-pos))
                 all-break-positions))
     
     ;; Check the final line - if it's short, apply pseudo-indent at the start of that line
     (let ((last-break-pos (if (null? all-break-positions) 0 (list-ref all-break-positions (- (length all-break-positions) 1)))))
       (let ((final-line-length (- total-measures last-break-pos)))
         (if (< final-line-length 4)
             (set! short-line-positions (append short-line-positions (list last-break-pos))))))
     
     ;; Generate pseudo-indent music at each short line position
     (ly:message "=== APPLYING PSEUDO-INDENTS ===")
     (ly:message "Short line positions to apply: ~a" short-line-positions)
     (for-each (lambda (pos)
                 (ly:message "Applying pseudo-indent at position ~a" pos)
                 (set! result (append result (list (create-pseudo-indent-music (get-pseudo-indent-value 'default))))))
               short-line-positions)
     (ly:message "Applied ~a pseudo-indents total" (length short-line-positions))
     (ly:message "================================")
     
     ;; Return the result as sequential music
     (if (null? result)
         (make-music 'SequentialMusic 'elements '())
         (make-music 'SequentialMusic 'elements result))))


% Combined function that generates breaks AND applies pseudo-indents at the right positions
autoBreaksAndPseudoIndents = #(define-music-function (marks) (ly:music?)
   "Generate breaks and automatically apply pseudo-indents to short lines"
   (let ((all-break-positions '())
         (current-position 0)
         (current-section-measures 0)
         (total-measures 0)
         (short-line-positions '())
         (short-lines-with-lengths '())
         (result '())
         (previous-pos 0))
     
     ;; First, analyze the marks to get all break positions and short line positions
     (define (process-element element)
       (cond
        ((eq? (ly:music-property element 'name) 'AdHocMarkEvent)
         (set! all-break-positions (append all-break-positions (list current-position)))
         (let ((section-four-measure-breaks '()))
           (do ((measure 4 (+ measure 4)))
               ((> measure current-section-measures))
             (set! section-four-measure-breaks (append section-four-measure-breaks (list (+ current-position measure)))))
           (set! all-break-positions (append all-break-positions section-four-measure-breaks)))
         (set! current-position (+ current-position current-section-measures))
         (set! current-section-measures 0))
        
        ((eq? (ly:music-property element 'name) 'SkipEvent)
         (let* ((duration (ly:music-property element 'duration))
                (measures (if duration (ly:duration-scale duration) 1)))
           (set! current-section-measures (+ current-section-measures measures))))))
     
     (if (eq? (ly:music-property marks 'name) 'SequentialMusic)
         (for-each process-element (ly:music-property marks 'elements))
         (process-element marks))
     
     (set! all-break-positions (append all-break-positions (list current-position)))
     (let ((final-section-four-measure-breaks '()))
       (do ((measure 4 (+ measure 4)))
           ((> measure current-section-measures))
         (set! final-section-four-measure-breaks (append final-section-four-measure-breaks (list (+ current-position measure)))))
       (set! all-break-positions (append all-break-positions final-section-four-measure-breaks)))
     
     (set! all-break-positions (sort (delete-duplicates all-break-positions) <))
     
     ;; Count total measures
     (define (count-total-measures element)
       (cond
        ((eq? (ly:music-property element 'name) 'SkipEvent)
         (let* ((duration (ly:music-property element 'duration))
                (measures (if duration (ly:duration-scale duration) 1)))
           (set! total-measures (+ total-measures measures))))))
     
     (if (eq? (ly:music-property marks 'name) 'SequentialMusic)
         (for-each count-total-measures (ly:music-property marks 'elements))
         (count-total-measures marks))
     
     ;; Find short line positions and their lengths
     (let ((prev-pos 0))
       (for-each (lambda (current-pos)
                   (let ((line-length (- current-pos prev-pos)))
                     (if (and (< line-length 4) (> current-pos 0))
                         (set! short-lines-with-lengths (append short-lines-with-lengths (list (cons prev-pos line-length))))))
                   (set! prev-pos current-pos))
                 all-break-positions))
     
     ;; Check the final line
     (let ((last-break-pos (if (null? all-break-positions) 0 (list-ref all-break-positions (- (length all-break-positions) 1)))))
       (let ((final-line-length (- total-measures last-break-pos)))
         (if (< final-line-length 4)
             (set! short-lines-with-lengths (append short-lines-with-lengths (list (cons last-break-pos final-line-length)))))))
     
     ;; Extract just the positions for backward compatibility
     (set! short-line-positions (map car short-lines-with-lengths))
     
     ;; Set the global short-line-positions for alignment analysis
     (set! short-line-positions short-line-positions)
     
     (ly:message "=== COMBINED BREAKS AND PSEUDO-INDENTS ===")
     (ly:message "Break positions: ~a" all-break-positions)
     (ly:message "Short line positions: ~a" short-line-positions)
     (ly:message "Short lines with lengths: ~a" short-lines-with-lengths)
     
     ;; Use the global helper function for getting pseudo-indent values
     
     ;; Generate breaks and pseudo-indents in the right order
     (for-each (lambda (pos)
                 ;; Add skip for measures between previous position and current position
                 (let ((measures-to-skip (- pos previous-pos)))
                   (if (> measures-to-skip 0)
                       (set! result (append result (list (make-music 'SkipEvent 'duration (ly:make-duration 0 0 measures-to-skip)))))))
                 
                 ;; Check if this position needs a pseudo-indent (is start of a short line)
                 (let ((line-info (assoc pos short-lines-with-lengths)))
                   (if line-info
                       (let ((line-length (cdr line-info)))
                         (ly:message "Adding pseudo-indent at position ~a (line length: ~a)" pos line-length)
                         (set! result (append result (list (create-pseudo-indent-music (get-pseudo-indent-value line-length))))))))
                 
                 ;; Add break
                 (set! result (append result (list (make-music 'LineBreakEvent 'break-permission 'force))))
                 (set! previous-pos pos))
               all-break-positions)
     
     (ly:message "================================")
     
     ;; Create the result as sequential music
     (make-music 'SequentialMusic 'elements result)))