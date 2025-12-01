% Score Layout Settings
% This file contains score-specific layout overrides and callbacks

% Include required dependencies
\include "../third-party/esmuflily/ly/cosmufl.ily"
\include "../staff/thin-staff-lines.ly"
\include "../rehearsal-marks/capsule-markers.ly"

% Score layout overrides
\layout {
  \context {
    \Score
      % Enable SMuFL support for Bravura font
      \ekmSmuflOn #'all
      % \override SpacingSpanner.packed-spacing = ##t
      % Make staff lines thinner using custom stencil
      \override StaffSymbol.stencil = #thin-staff-lines
      % Bar number settings
      \override BarNumber.font-size = -5  % Make bar numbers smaller
      \override BarNumber.Y-offset = #4.5  % Move bar numbers to the right
      \override BarNumber.X-offset = #0.4  % Move bar numbers to the right
      % Rehearsal mark settings
      \override RehearsalMark.break-align-symbols = #'(left-edge)
      \override RehearsalMark.self-alignment-X = #RIGHT
      \override RehearsalMark.self-alignment-Y = #UP
      \override RehearsalMark.before-line-breaking = #capsule-marker

  }
}
