% Font configuration for LilyPond charting workflow
% This file contains font setup and configuration

% Set up custom fonts
#(ly:font-config-add-directory "fonts/San-Francisco-Pro-Fonts-master/")
#(ly:font-config-add-directory "fonts/musescore/fonts/musejazz/")

% Global font settings
\paper {
  #(define fonts
    (set-global-fonts
     #:roman "LilyJazz Text"
     #:sans "LilyJazz Text"
     #:typewriter "LilyJazz Text"
    ))
}
