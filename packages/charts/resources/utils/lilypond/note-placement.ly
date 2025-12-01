% Note placement utilities for LilyPond charting workflow
% This file contains functions for automatically placing text cues at specific measures

% Markup template for note cues
\markup noteMarkup = \markup \fontsize #-1 \override #'(font-name . "MuseJazz Text") \underline \etc

% Function to generate text cues with automatic spacing between measures
% Automatically detects total measures needed from the cue list
% Supports beat positioning within measures and color override
#(define (generate-cues-with-spacing cue-list color)
   (let* ((result '())
          (current-measure 1)
          (current-beat 0)
          ;; Find the last measure in the cue list
          (last-measure (if (null? cue-list) 
                           1 
                           (apply max (map car cue-list)))))
     (for-each
      (lambda (cue)
        (let* ((target-measure (car cue))
               (cue-data (cdr cue))
               ;; Check if cue-data is a list (measure . (beat . text)) or just text
               (target-beat (if (pair? cue-data) (car cue-data) 0))
               (text (if (pair? cue-data) (cdr cue-data) cue-data)))
          ;; Add silence to reach the target measure and beat
          (let ((measures-to-add (- target-measure current-measure)))
            ;; Add full measures if needed (but not if we're already at the target measure)
            (if (> measures-to-add 0)
                (set! result (append result 
                  (list (make-music 'SkipEvent 
                        'duration (ly:make-duration 0 0 measures-to-add))))))
            ;; Add partial measure for beat positioning (only if we're in the same measure)
            (if (and (= measures-to-add 0) (> target-beat current-beat))
                (let ((beat-duration (ly:make-duration 2 0 (- target-beat current-beat))))
                  (set! result (append result 
                    (list (make-music 'SkipEvent 'duration beat-duration))))))
            ;; Add the text cue with inline markup and color (using SkipEvent for silence)
            (set! result (append result 
              (list 
                (make-music 'SkipEvent 'duration (ly:make-duration 0 0 1/1)
                  'articulations (list 
                    (make-music 'TextScriptEvent 'direction -1
                      'text (markup #:fontsize 0.3 #:override '(font-name . "MuseJazz Text") #:with-color color #:underline text)))))))
            ;; Update current position to the measure AFTER the text cue
            (set! current-measure (+ target-measure 1))
            (set! current-beat 0))))  ; Reset to beat 0 after each text cue
      cue-list)
     ;; Add remaining silence to reach the last measure
     (if (< current-measure last-measure)
         (set! result (append result 
           (list (make-music 'SkipEvent 
                 'duration (ly:make-duration 0 0 
                   (- last-measure current-measure)))))))
     (make-music 'SequentialMusic 'elements result)))

% Music function to create text cues at specific measures
% Usage: \textCuesAtMeasures #'((27 . "Intro Groove!") (76 . "Band Hits!")) "red"
% Or: \textCuesAtMeasures #'((27 . "Intro Groove!") (76 . "Band Hits!")) #(x11-color 'DarkGreen)
textCuesAtMeasures = 
#(define-music-function (cue-list color)(list? color?)
   "Create text cues at specific measures with automatic spacing and color"
   (generate-cues-with-spacing cue-list color))

% Inline text cue function for immediate placement
% Usage: \cue "all" "Intro Groove!" or \cue "keys" "Arps!"
cue = 
#(define-music-function (group message)(string? string?)
   "Create an inline text cue with automatic color based on instrument group"
   (let* ((group-lower (string-downcase group))
          (color (cond
                   ((string=? group-lower "all") "red")      ; Red-orange
                   ((string=? group-lower "drums") "orange")     ; Orange  
                   ((string=? group-lower "bass") "purple")  ; Purple
                   ((string=? group-lower "guitar") "blue") ; Blue
                   ((string=? group-lower "keys") "green")   ; Green
                   ((string=? group-lower "vocals") "magenta")        ; Magenta
                   (else "gray")))) ; Gray for custom
     (make-music 'TextScriptEvent 'direction -1
       'text (markup #:fontsize 0.3 #:override '(font-name . "MuseJazz Text") #:with-color color #:underline message))))
