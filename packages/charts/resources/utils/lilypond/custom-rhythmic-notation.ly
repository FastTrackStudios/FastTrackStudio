\version "2.24.0"

% Custom Rhythmic Notation Utilities
% Provides intelligent slash notation with automatic stem hiding for consecutive quarter notes

\include "english.ly"

% Requires esmuflily for SMuFL glyphs
ekmFont = "Bravura"
% Note: esmufl.ily should be included in the main document

% ===== PSEUDO-INDENT HELPERS =====

% Constants for pseudo-indent values based on line length
#(define pseudo-indent-values
   '((1 . 70)   ; 1 measure = pseudo-indent 70
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

% ===== CLEF DETECTION =====

detectClef = #(define-music-function () ()
  "Detect and print the current clef value"
  (make-apply-context
    (lambda (context)
      (let ((clefGlyph (ly:context-property context 'clefGlyph))
            (middleCPosition (ly:context-property context 'middleCPosition)))
        (make-music 'Music 'void #t)))))

#(define clef-pitch #f)

#(define (set-clef-pitch context)
  (let ((clefGlyph (ly:context-property context 'clefGlyph)))
    (set! clef-pitch 
          (cond
           ((string=? clefGlyph "clefs.G")
            (ly:make-pitch 0 6 0))
           ((string=? clefGlyph "clefs.F")
            (ly:make-pitch -1 1 0))
           (else
            (ly:make-pitch 0 0 0))))))

% ===== MIDDLE LINE NOTE FUNCTION =====

middleLineNote = #(define-music-function (music) (ly:music?)
  "Apply clef-aware pitch to rhythm notes"
  (let ((pitch-set #f))
    #{
      \applyContext #(lambda (context)
        (let* ((clefGlyph (ly:context-property context 'clefGlyph))
               (target-pitch (cond
                              ((string=? clefGlyph "clefs.G")
                               (ly:make-pitch 0 6 0))
                              ((string=? clefGlyph "clefs.F")
                               (ly:make-pitch -1 1 0))
                              (else
                               (ly:make-pitch 0 0 0)))))
          (music-map (lambda (m)
                      (if (music-is-of-type? m 'note-event)
                          (begin
                            (ly:music-set-property! m 'pitch target-pitch)
                            m)
                          m))
                    music)))
      #music
    #}))

% ===== CUSTOM NOTEHEAD STYLES =====

#(define-public diamond-slash-style
  (lambda (grob)
    (let* ((duration-log (ly:grob-property grob 'duration-log)))
      (grob-interpret-markup grob (make-ekm-char-markup #xE104)))))

#(define-public regular-slash-style
  (lambda (grob)
    (grob-interpret-markup grob (make-ekm-char-markup #xE103))))

% Dynamic notehead based on duration
#(define (duration-notehead grob)
  (let* ((duration-log (ly:grob-property grob 'duration-log))
         (glyph (cond
                 ((<= duration-log -1) #xE104)  ; breve and longer - diamond slash
                 ((= duration-log 0) #xE104)    ; whole note - diamond slash
                 ((= duration-log 1) #xE104)    ; half note - diamond slash
                 ((= duration-log 2) #xE101)    ; quarter note - slash with horizontal ends
                 ((= duration-log 3) #xE101)    ; eighth note - slash with horizontal ends
                 ((= duration-log 4) #xE101)    ; sixteenth note - slash with horizontal ends
                 ((= duration-log 5) #xE101)    ; thirty-second note - slash with horizontal ends
                 ((= duration-log 6) #xE101)    ; sixty-fourth note - slash with horizontal ends
                 (else #xE101))))
    ; Set stem properties based on notehead type
    (let ((stem (ly:grob-object grob 'stem)))
      (if stem
          (begin
            (if (or (<= duration-log -1) (= duration-log 0) (= duration-log 1))
                (begin
                  (ly:grob-set-property! stem 'Y-offset 0.4)  ; diamond slash noteheads
                  (ly:grob-set-property! stem 'neutral-direction down)
                  ; Extend stem length for half notes
                  (if (= duration-log 1)
                      (ly:grob-set-property! stem 'length 7.0)))
                (begin
                  (ly:grob-set-property! stem 'Y-offset -0.7)  ; regular slash noteheads
                  (ly:grob-set-property! stem 'neutral-direction DOWN))))))
    (grob-interpret-markup grob (make-ekm-char-markup glyph))))

% ===== QUARTER NOTE ANALYSIS =====

#(define bar-analysis #f)
#(define current-bar-notes (list))
#(define note-counter 0)

#(define (analyze-quarter-notes music)
  (set! current-bar-notes (list))
  (set! note-counter 0)
  (let ((result (music-map (lambda (m)
                            (cond
                             ((music-is-of-type? m 'note-event)
                              (let* ((duration (ly:music-property m 'duration))
                                     (duration-log (ly:duration-log duration))
                                     (dots (ly:duration-dot-count duration))
                                     (is-quarter (and (= duration-log 2) (= dots 0))))
                                ; Store note info with reference to music object
                                (set! current-bar-notes (append current-bar-notes (list (list m duration-log dots is-quarter))))
                                m))
                             ((eq? (ly:music-property m 'name) 'BarCheck)
                              ; Process this bar to mark consecutive quarter notes as stemless
                              (let ((consecutive-quarters (list)))
                                (for-each (lambda (note-info)
                                           (let ((music-obj (car note-info))
                                                 (duration-log (cadr note-info))
                                                 (dots (caddr note-info))
                                                 (is-quarter (cadddr note-info)))
                                             (if is-quarter
                                                 (set! consecutive-quarters (append consecutive-quarters (list music-obj)))
                                                 (begin
                                                   ; End of consecutive quarters, mark them if 2 or more
                                                   (if (>= (length consecutive-quarters) 2)
                                                       (for-each (lambda (m) 
                                                                  (ly:music-set-property! m 'stemless #t))
                                                                consecutive-quarters))
                                                   (set! consecutive-quarters (list))))))
                                         current-bar-notes)
                                ; Process any remaining consecutive quarters at end of bar
                                (if (>= (length consecutive-quarters) 2)
                                    (for-each (lambda (m) 
                                               (ly:music-set-property! m 'stemless #t))
                                             consecutive-quarters)))
                              (set! current-bar-notes (list))
                              m)
                             (else m)))
                          music)))
    ; Process any remaining notes after all music (final bar without bar check)
    (if (> (length current-bar-notes) 0)
        (begin
          (let ((consecutive-quarters (list)))
            (for-each (lambda (note-info)
                       (let ((music-obj (car note-info))
                             (duration-log (cadr note-info))
                             (dots (caddr note-info))
                             (is-quarter (cadddr note-info)))
                         (if is-quarter
                             (set! consecutive-quarters (append consecutive-quarters (list music-obj)))
                             (begin
                               ; End of consecutive quarters, mark them if 2 or more
                               (if (>= (length consecutive-quarters) 2)
                                   (for-each (lambda (m) 
                                              (ly:music-set-property! m 'stemless #t))
                                            consecutive-quarters))
                               (set! consecutive-quarters (list))))))
                     current-bar-notes)
            ; Process any remaining consecutive quarters at end of final bar
            (if (>= (length consecutive-quarters) 2)
                (for-each (lambda (m) 
                           (ly:music-set-property! m 'stemless #t))
                         consecutive-quarters)))))
    result))

% ===== DYNAMIC STEM FUNCTION =====

#(define (duration-stem grob)
  (let* ((note-heads-array (ly:grob-object grob 'note-heads))
         (note-heads (if (ly:grob-array? note-heads-array)
                         (ly:grob-array->list note-heads-array)
                         '()))
         (note-head (if (pair? note-heads) (car note-heads) #f))
         (event (if note-head (ly:grob-property note-head 'cause) #f))
         (stemless (if (ly:stream-event? event)
                       (ly:event-property event 'stemless #f)
                       #f)))
    (if stemless
        (ly:make-stencil "" '(0 . 0) '(0 . 0))
        (ly:stem::print grob))))

% ===== SKIP EXPANSION =====

#(define (expand-skips music)
  "Expand skip events (s1, s2, etc.) into quarter notes"
  (music-map (lambda (m)
              (if (music-is-of-type? m 'skip-event)
                  (let* ((duration (ly:music-property m 'duration))
                         (length (ly:duration-length duration))
                         (quarter-length (ly:make-duration 2 0))
                         (num-quarters (inexact->exact (round (/ (ly:moment-main length) 1/4)))))
                    (make-sequential-music
                     (map (lambda (i)
                           (make-music 'NoteEvent
                                      'duration quarter-length
                                      'pitch (ly:make-pitch 0 0 0)))
                         (iota num-quarters))))
                  m))
            music))

% ===== MAIN RHYTHMIC NOTATION FUNCTION =====

rh = #(define-music-function (music) (ly:music?)
  "Create slash notes with dynamic noteheads, intelligent stem hiding, and skip expansion.
   
   Usage examples:
     \\rh { 4 4 4 4 }         % Four quarter notes (all stemless)
     \\rh { 4 4 8 8 4 }       % Mixed durations (stems appear as needed)
     \\rh { s1*4 }            % Expands to 16 quarter notes across 4 bars
     \\rh { 4. 8 2 }          % Dotted quarter, eighth, half note"
  (let ((expanded-music (expand-skips music)))
    #{
      \override NoteHead.stencil = #duration-notehead
      \override Stem.stencil = #duration-stem
      \override Accidental.stencil = ##f
      \override Stem.details.beamed-lengths = #'(3)
      \applyMusic #analyze-quarter-notes
      \middleLineNote #expanded-music
      \revert NoteHead.stencil
      \revert Stem.stencil
      \revert Accidental.stencil
    #}))

% ===== CHART LAYOUT HELPER =====

chartLayout = #(define-music-function (marks-music content-music) (ly:music? ly:music?)
  "Combine marks and content with auto-breaks and pseudo-indents.
   Automatically duplicates marks for break calculation.
   
   Usage:
     \\chartLayout {
       \\mark \"CH 1\"
       s1*6 |
       \\mark \"VS 2\"
       s1*4 |
     } {
       \\rh { 4 4 4. 8 | s1*5 }
       \\rh { s1*4 }
     }"
  (let ((marks-copy (ly:music-deep-copy marks-music)))
    #{
      <<
        #marks-music
        #content-music
        #(autoBreaksAndPseudoIndents marks-copy)
      >>
    #}))

% ===== AUTO INLINE BREAKS =====

#(define section-measure-counts '())
#(define current-section-measures 0)

autoInlineBreaks = #(define-music-function (music) (ly:music?)
  "Automatically add breaks at marks by calculating measure durations.
   Inserts a break before each mark (after the previous section)."
  (set! section-measure-counts '())
  (set! current-section-measures 0)
  
  ; Helper to get duration in whole notes
  (define (get-duration-length element)
    (let ((duration (ly:music-property element 'duration)))
      (if (ly:duration? duration)
          (ly:moment-main (ly:duration-length duration))
          0)))
  
  ; Process elements in order, calculating measures from durations
  (define (process-element element)
    (let ((name (ly:music-property element 'name)))
      (cond
       ((eq? name 'AdHocMarkEvent)
        ; Save current section measure count
        (ly:message "Mark found - section had ~a measures" current-section-measures)
        (set! section-measure-counts (append section-measure-counts (list current-section-measures)))
        ; Reset for new section
        (set! current-section-measures 0))
       
       ; Count duration from note, rest, and skip events
       ((or (eq? name 'NoteEvent)
            (eq? name 'RestEvent)
            (eq? name 'SkipEvent))
        (let ((duration-length (get-duration-length element)))
          ; Assuming 4/4 time, convert duration to measures (1 whole note = 1 measure)
          (set! current-section-measures (+ current-section-measures duration-length))
          (ly:message "~a event: duration=~a, section total=~a measures" 
                      name duration-length current-section-measures))))
      
      ; Recursively process sub-elements
      (let ((elements (ly:music-property element 'elements))
            (sub-element (ly:music-property element 'element)))
        (if (pair? elements)
            (for-each process-element elements))
        (if (ly:music? sub-element)
            (process-element sub-element)))))
  
  ; Process the music
  (process-element music)
  
  ; Add final section measure count
  (ly:message "Final section had ~a measures" current-section-measures)
  (set! section-measure-counts (append section-measure-counts (list current-section-measures)))
  
  (ly:message "=== SECTION ANALYSIS ===")
  (ly:message "Section measure counts: ~a" section-measure-counts)
  
  ; Calculate break positions including both section breaks and 4-measure breaks
  (let ((all-break-positions '())
        (current-position 0))
    
    ; Process each section - track section breaks separately
    (let ((section-breaks '())
          (four-measure-breaks '()))
      (for-each (lambda (section-measures)
                  ; Add section break at current position (if not at 0)
                  (if (> current-position 0)
                      (begin
                        (ly:message "Section break at position ~a" current-position)
                        (set! section-breaks (append section-breaks (list current-position)))))
                  
                  ; Generate four-measure breaks within this section
                  (do ((measure 4 (+ measure 4)))
                      ((>= measure section-measures))
                    (begin
                      (ly:message "4-measure break at position ~a" (+ current-position measure))
                      (set! four-measure-breaks 
                            (append four-measure-breaks 
                                    (list (+ current-position measure))))))
                  
                  ; Update position for next section
                  (set! current-position (+ current-position section-measures)))
                section-measure-counts)
      
      ; Combine all breaks: section breaks + four-measure breaks
      (set! all-break-positions (append section-breaks four-measure-breaks)))
    
    ; Remove duplicates and sort
    (set! all-break-positions (sort (delete-duplicates all-break-positions) <))
    
    (ly:message "All break positions (before filtering): ~a" all-break-positions)
    
    ; Remove only the very last position (end of entire score), not section breaks
    (let ((total-measures (apply + section-measure-counts)))
      (set! all-break-positions (filter (lambda (pos) (< pos total-measures)) all-break-positions)))
    
    (ly:message "Final break positions: ~a" all-break-positions)
    
    ; Calculate line lengths for each break position (same logic as auto-pseudo-indents.ly)
    (let ((short-lines-with-lengths '()))
      ; Check the first line (from position 0 to first break) FIRST
      (if (pair? all-break-positions)
          (let ((first-break-pos (car all-break-positions)))
            (if (< first-break-pos 4)
                (set! short-lines-with-lengths (append short-lines-with-lengths (list (cons 0 first-break-pos)))))))
      
      (let ((prev-pos 0))
        (for-each (lambda (current-pos)
                    (let ((line-length (- current-pos prev-pos)))
                      ; Only add if not position 0 (first line handled separately)
                      (if (and (< line-length 4) (> current-pos 0) (> prev-pos 0))
                          (set! short-lines-with-lengths (append short-lines-with-lengths (list (cons prev-pos line-length))))))
                    (set! prev-pos current-pos))
                  all-break-positions))
      
      ; Check the final line
      (let* ((total-measures (apply + section-measure-counts))
             (last-break-pos (if (null? all-break-positions) 0 (car (reverse all-break-positions))))
             (final-line-length (- total-measures last-break-pos)))
        (if (< final-line-length 4)
            (set! short-lines-with-lengths (append short-lines-with-lengths (list (cons last-break-pos final-line-length))))))
      
      (ly:message "Short lines with lengths: ~a" short-lines-with-lengths)
      
      ; Generate break events with pseudo-indents (like auto-pseudo-indents.ly)
      (if (pair? all-break-positions)
          (let ((result '())
                (previous-pos 0))
            ; First, handle position 0 (start of score) if it needs pseudo-indent
            (let ((line-info (assoc 0 short-lines-with-lengths)))
              (if line-info
                  (let ((line-length (cdr line-info)))
                    (ly:message "*** ADDING PSEUDO-INDENT at position 0 (line length: ~a) ***" line-length)
                    (ly:message "Pseudo-indent value: ~a" (get-pseudo-indent-value line-length))
                    (set! result (append result (list (create-pseudo-indent-music (get-pseudo-indent-value line-length))))))))
            
            (for-each (lambda (pos)
                        ; Add skip for measures between breaks
                        (let ((measures-to-skip (- pos previous-pos)))
                          (if (> measures-to-skip 0)
                              (begin
                                (ly:message "Adding skip for ~a measures" measures-to-skip)
                                (set! result (append result (list (make-music 'SkipEvent 'duration (ly:make-duration 0 0 measures-to-skip))))))))
                        
                        ; Check if this position starts a short line
                        (let ((line-info (assoc pos short-lines-with-lengths)))
                          (ly:message "Checking position ~a for pseudo-indent..." pos)
                          (ly:message "Line info: ~a" line-info)
                          (if line-info
                              ; Use helper functions from auto-pseudo-indents.ly
                              (let ((line-length (cdr line-info)))
                                (ly:message "*** ADDING PSEUDO-INDENT at position ~a (line length: ~a) ***" pos line-length)
                                (ly:message "Pseudo-indent value: ~a" (get-pseudo-indent-value line-length))
                                ; Use create-pseudo-indent-music which calls get-pseudo-indent-value
                                (set! result (append result (list (create-pseudo-indent-music (get-pseudo-indent-value line-length))))))
                              ; No pseudo-indent needed, just add a regular break
                              (begin
                                (ly:message "No pseudo-indent needed at position ~a, adding regular break" pos)
                                (set! result (append result (list (make-music 'LineBreakEvent 'break-permission 'force)))))))
                        (set! previous-pos pos))
                      all-break-positions)
          
          ; Layer original music with breaks and pseudo-indents
          #{
            <<
              #music
              #(make-music 'SequentialMusic 'elements result)
            >>
          #})
        ; No breaks needed, just return music
        music))))

