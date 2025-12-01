%% http://lsr.di.unimi.it/LSR/Item?id=775

% here starts the snippet:

% Contributed by harm6
% Code is taken from output-lib.scm and modified.
% This snippet was developed with 2.14.2, but there is no difficulty with 2.12.2
% Several other Definitions and functions are possible.

%------------ Definitions

#(define (color-at-line-begin g)
  (ly:message "COLOR-AT-LINE-BEGIN called on grob: ~a" (ly:grob-name g))
  (if (and (ly:item? g)
	   (equal? (ly:item-break-dir g) RIGHT))
      (begin
        (ly:message "COLORING grob ~a RED at line begin" (ly:grob-name g))
        (ly:grob-set-property! g 'color red))
      (ly:message "NOT coloring grob ~a - break-dir: ~a" (ly:grob-name g) (ly:item-break-dir g))))
      
#(define (rotate-at-line-begin g)
  (if (and (ly:item? g)
	   (equal? (ly:item-break-dir g) RIGHT))
      (ly:grob-set-property! g 'rotation (list 145 0 0))))
      
#(define (translate-at-line-begin g)
  (if (and (ly:item? g)
	   (equal? (ly:item-break-dir g) RIGHT))
      (and (ly:grob-translate-axis! g -3.5 Y)
           (ly:grob-translate-axis! g -3.5 X))
      ))
      
#(define (color-at-line-end g)
  (ly:message "COLOR-AT-LINE-END called on grob: ~a" (ly:grob-name g))
  (if (and (ly:item? g)
	   (equal? (ly:item-break-dir g) LEFT))
      (begin
        (ly:message "COLORING grob ~a RED at line end" (ly:grob-name g))
        (ly:grob-set-property! g 'color red))
      (ly:message "NOT coloring grob ~a - break-dir: ~a" (ly:grob-name g) (ly:item-break-dir g))))
         
#(define (rotate-at-line-end g)
  (if (and (ly:item? g)
	   (equal? (ly:item-break-dir g) LEFT))
      (ly:grob-set-property! g 'rotation (list 45 0 0))))
      
%---------- music-functions
 
% In the music-functions colors are to be specified like rgb-colors.

colorBarLineBeg =      
#(define-music-function (color)(list?)
 (define (color-at-line-begin g)
  (if (and (ly:item? g)
	   (equal? (ly:item-break-dir g) RIGHT))
      (ly:grob-set-property! g 'color color)))
#{
        \once\override Staff.BarLine.after-line-breaking = #color-at-line-begin
#})

colorBarLineEnd =      
#(define-music-function (color)(list?)
 (define (color-at-line-end g)
  (if (and (ly:item? g)
	   (equal? (ly:item-break-dir g) LEFT))
      (ly:grob-set-property! g 'color color)))
#{
        \once\override Staff.BarLine.after-line-breaking = #color-at-line-end
#})