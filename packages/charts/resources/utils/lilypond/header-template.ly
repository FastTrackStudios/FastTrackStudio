% Header template for LilyPond charting workflow
% This file contains a reusable header template with variables

% Define default values for header variables
% These can be overridden in the main file
#(define songTitle "Song Title")
#(define masterRhythm "Master Rhythm")
#(define transcribedBy "Transcribed By")
#(define transcriberName "Cody Wright")
#(define tempoMark "♩ = 120")

\header {
  title = #songTitle
  subtitle = #masterRhythm
  composer = #transcribedBy
  arranger = #transcriberName
  tagline = \markup { \with-color #grey "Formatted with FTS-Extensions " }
}

% Custom title layout with part type in top left
\paper {
  scoreTitleMarkup = \markup {
    \fill-line {
      \column {
        \line { \fontsize #1 \override #'(font-family . "SF Pro Display Bold") "MASTER" }
        \line { \fontsize #1 \override #'(font-family . "SF Pro Display Bold") "RHYTHM" }
        \line { \fontsize #0.5 \with-color #grey \override #'(font-family . "SF Pro Text") "V1" }
        \vspace #1
         \fontsize #2 \override #'(font-family . "SF Pro Display Medium") \translate #'(3 . 0) #tempoMark
      }
      \center-column {
        \fontsize #10 \override #'(font-family . "SF Pro Display Bold") \fromproperty #'header:title
        \fontsize #-4 \line { \override #'(font-family . "SF Pro Display Thin Italic") "Transcribed by: " \override #'(font-family . "SF Pro Display Medium") \fromproperty #'header:arranger }
      }
      \column {
        \line { \fontsize #-1 \override #'(font-family . "SF Pro Text") "Credits 1" }
        \line { \fontsize #-1 \override #'(font-family . "SF Pro Text") "Credits 2" }
      }
    }
  }
}
