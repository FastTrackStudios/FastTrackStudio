% Music definitions for LilyPond charting workflow
% This file contains reusable music expressions and definitions

% Rhythmic slash notation
oneLineSlashes = {
  \override NoteHead.style = #'slash
  \hide Stem
  b1
}

% Chord progression
chordProgression = {
  \chordmode {
    c1:7 | f1:7.9+ | d1:m7 | g1:7 |
    c1:7 | f1:7.9+ | d1:m7 | g1:7 |
  }
}

% Main music
mainMusic = {
  \oneLineSlashes
  \break
  \oneLineSlashes
  \break
  \oneLineSlashes
  \break
  \oneLineSlashes
  \break
  \oneLineSlashes
  \break
  \oneLineSlashes
  \break
  \oneLineSlashes
  \break
  \oneLineSlashes
  \break
  \oneLineSlashes
}
