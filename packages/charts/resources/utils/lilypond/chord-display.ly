% Chord display configuration for LilyPond charting workflow
% This file contains chord name display settings and customizations

% Custom chord root namer to use MuseJazz sharp and flat symbols
#(define (musejazz-chord-name->markup pitch context)
  (let* ((alt (ly:pitch-alteration pitch)))
    (make-line-markup
     (list
      (make-simple-markup (vector-ref #("C" "D" "E" "F" "G" "A" "B")
                                      (ly:pitch-notename pitch)))
      ;; If it's natural, do nothing
      (if (= alt 0)
          (make-line-markup (list empty-markup))
          (if (= alt FLAT)
              ;; Handle adding the flat symbol with MuseJazz styling
              (make-line-markup
               (list
                (make-hspace-markup 0)
                (make-raise-markup 0
                 (make-fontsize-markup 0
                  (make-text-markup "♭")))
                ))
              ;; Handle adding the sharp symbol with MuseJazz styling
              (make-line-markup
               (list
                (make-hspace-markup 0)
                (make-raise-markup 0
                 (make-fontsize-markup 0 
                  (make-text-markup "♯")))
                ))
              ))))))

% Custom chord flat and sharp symbols for MuseJazz
chordFlat = \markup { \hspace #0.2 \fontsize #-1 \raise #0.1 "b" }
chordSharp = \markup { \hspace #0.1 \fontsize #-1 \lower #0.3 "#" }

% Custom chord name exceptions to display "sus" instead of "sus4" and "maj7" instead of triangle
% Define exceptions that auto-expand to every root (the #t flag)
chordExceptionsMusic = {
  % Sus chord exceptions - order matters, more specific first
  <c f g a'>1              -\markup { \super "13" "sus" }    % C11sus4 -> C11sus
  <c f g bf>1             -\markup { \super "7" "sus" }     % C7sus4 -> C7sus
  <c f g>1                 -\markup { "sus" }      % Csus4 -> Csus
  
  % Diminished chord exceptions - try different voicings
  <c ef gf bf>1            -\markup { "dim" }      % Cdim7 -> Cdim
  <c ef gf>1               -\markup { "dim" }      % Cdim5 -> Cdim
  <c ef gf a>1             -\markup { "dim" }      % Cdim7 -> Cdim (alternative voicing)
  <c ef gf bf d'>1         -\markup { "dim" }      % Cdim9 -> Cdim
  
  % Major 7 chord exceptions - replace triangle with "maj7"
  <c e g b>1               -\markup { "maj" \super "7" }     % C△ -> Cmaj7
  <c e g b d'>1            -\markup { "maj" \super "9" }     % C△9 -> Cmaj9
  <c e g b d' a'>1         -\markup { "maj" \super "11" }    % C△11 -> Cmaj11
  <c e g b d' a' c''>1     -\markup { "maj" \super "13" }    % C△13 -> Cmaj13
}

chordExceptions =
#(append
   (sequential-music-to-chord-exceptions chordExceptionsMusic #t)
   ignatzekExceptions)
