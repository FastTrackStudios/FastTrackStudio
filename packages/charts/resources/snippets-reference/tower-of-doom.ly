%% Thabks to Arnold from the geman forum
%% https://lilypondforum.de/index.php/topic,1216.msg6404.html#msg6404

#(define-markup-command (pop-override layout props arg)
  (markup?)
  "pop the last override in the markup chain"
  (let ((popped-props (if (> (length props) 1) (cdr props) props)))
   (interpret-markup layout popped-props arg)))

#(define-markup-command (slant layout props phi arg)
  (number? markup?)
  #:category other
  #:properties ((par #t)
                (tilt 0.0)
                (finalrotation 0.0)
                (extrascaling '(1 . 1)))
  "
A geometric transformation which may be described in two ways:
 a) Shear Mapping: Cut your image in small stripes parallel to your
    base axis (e.g. X axis), then move them parallel to your base axis,
    the distance is proportional to the distance from the base axis.
 b) Skew Coordinates: See your input image as a geometric list by the
    cartesian coordiate system relative to your base axes (e.g. X axis
    and perpendicular the Y axis) - name these axes U resp. V. Now the
    V axis will be rotated to manipulate your image. Your U-V coordiante
    system is no longer cartesian, because it's no longer pependicular.
 Tranformation matrix for an angle @var{phi}, default tilt, type a
 resp. property par = ##t:
    xx = 1.0    xy = tan(w)
    yx = 0.0    yy = 1.0
 Tranformation matrix for an angle @var{phi}, default tilt, type b
 resp. property par = ##f:
    xx = 1.0    xy = sin(w)
    yx = 0.0    yy = cos(w)
 Relation from type b to type a: the original Y axis (V axis) is
 scaled by factor 1/cos(w)

 Properties for this markup:
   @var{par} is #t for Type a (default), #f for Type b
   @var{tilt} specifies the invariant axis, 0 for X axis (default),
   90 for Y axis, other values possible
   @var{finalrotation} specifies an additional rotation appended to this
   shear / skew mapping
   @var{extrascaling} will apply an extra scaling to the U and V axis
   
 Arguments for this markup:
   @var{phi} is the shear angle. (allmost) ±90° will force geometric
   problems.
   And finally the markup to be transformed.
   
Technically there is no SCHEME function available to apply a
ly:transform? object to a stencil. Therefore roation and scale
statements are used to fullfill the task. 
Once the U and V axis are properly scaled, the stencil is rotated so
the U axis directs 45° up to the right and the V axis directs 45° up to
the left. A properly choosen scale factor pair will rotate both axes
together (or away from each other) by a specified angle without scaling
these axes. A final back rotation completes the sequence.
   "
  (let* ((half-phi (* 0.5 phi))
         (phi-dir (ly:directed phi))
         (cos-phi (car phi-dir))
         (half-dir (ly:directed half-phi))
         (sin-part (cdr half-dir))
         (cos-part (car half-dir))
         (px (- cos-part sin-part))
         (py (+ cos-part sin-part))
         (stil (interpret-markup layout props arg))
         (need-first-rot (or par
                             (not (= (car extrascaling) 1))
                             (not (= (cdr extrascaling) 1))))
         (first-scale (if par
                       (cons (car extrascaling) (/ (cdr extrascaling) cos-phi))
                       extrascaling)))
   ; for debugging:
   ; (for-each display (list "\n\\slant " phi " (par=" par " ori=" tilt " fin=" finalrotation " scl=" extrascaling ")\n"
   ;  "  cos-phi=" (ly:number->string cos-phi)
   ;  " sin-part=" (ly:number->string sin-part)
   ;  " cos-part=" (ly:number->string cos-part)
   ;  " first-scale=" first-scale
   ;  "\n"))
   (if (< (abs cos-phi) 0.01) (ly:program-error
     "slant markup with angle too close to 90° may cause runtime error!"))
   (if (< (abs px) 0.00001)
    (set! px (if (negative? px) -0.00001 0.00001)))
   (if (< (abs py) 0.00001)
    (set! py (if (negative? py) -0.00001 0.00001)))
   (let* ((v-pos-stil
           (if need-first-rot
            (ly:stencil-rotate-absolute
             (ly:stencil-scale
              (if (= tilt 0.0)
               stil
               (ly:stencil-rotate-absolute
                stil
                (- tilt) 0.0 0.0))
              (car first-scale) (cdr first-scale))
             45.0 0.0 0.0)
            (ly:stencil-rotate-absolute stil (- 45.0 tilt) 0.0 0.0)))
          (final-stil
           (ly:stencil-rotate-absolute
            (ly:stencil-scale v-pos-stil px py)
            (+ -45.0 (- half-phi) tilt finalrotation) 0.0 0.0))
          (final-expr (ly:stencil-expr final-stil))
          (final-X-ext (stencil-true-extent final-stil X))
          (final-Y-ext (stencil-true-extent final-stil Y)))
    (ly:make-stencil final-expr final-X-ext final-Y-ext))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#(define di-wl 7.18)
#(define di-wr 41.41)
#(define bezier-round 0.551857)
#(define sz 16)

#(define (di-map size z x y)
  (let ((dir-l (ly:directed di-wl size))
        (dir-r (ly:directed di-wr size)))
   (cons (- (* (car dir-l) x) (* (car dir-r) 0.5 z))
         (- (* y size) (* (cdr dir-l) x) (* (cdr dir-r) 0.5 z)))))
         
DiRoofL = #(define-scheme-function (m) (markup?)
 (let* ((h (sqrt 0.75))
        (PP (di-map sz -0.5 0 h))
        (lgs (/ (sqrt (+ (* (car PP) (car PP)) (* (cdr PP) (cdr PP)))) sz))
        (w (ly:angle (car PP) (cdr PP))))
  (make-override-markup `((finalrotation . ,(- di-wl))
                          (extrascaling . (1 . ,lgs))
                          (par . #f))
   (make-slant-markup (- 90 w di-wl) m))))

DiRoofR = #(define-scheme-function (m) (markup?)
 (let* ((h (sqrt 0.75))
        (PP (di-map sz 0 -0.5 h))
        (lgs (/ (sqrt (+ (* (car PP) (car PP)) (* (cdr PP) (cdr PP)))) sz))
        (w (ly:angle (car PP) (cdr PP))))
  (make-override-markup `((finalrotation . ,di-wr)
                          (extrascaling . (0.5 . ,lgs))
                          (par . #f))
   (make-slant-markup (- 90 w (- di-wr)) m))))
   
DiPlaneL = #(define-scheme-function (m) (markup?)
 (make-override-markup `((finalrotation . ,(- di-wl))
                         (par . #f))
  (make-slant-markup (- di-wl) m)))

DiPlaneL¹ = #(define-scheme-function (m) (markup?)
 (make-override-markup `((finalrotation . 90)
                         (par . #f))
  (make-slant-markup di-wl m)))

DiPlaneR = #(define-scheme-function (m) (markup?)
 (make-override-markup `((finalrotation . ,di-wr)
                         (extrascaling . (0.5 . 1))
                         (par . #f))
  (make-slant-markup di-wr m)))
  
DiPlaneR¹ = #(define-scheme-function (m) (markup?)
 (make-override-markup `((finalrotation . 90)
                         (extrascaling . (1 . 0.5))
                         (par . #f))
  (make-slant-markup (- di-wr) m)))
  
DiPlaneT = #(define-scheme-function (m) (markup?)
 (make-override-markup `((finalrotation . ,(- di-wl))
                         (extrascaling . (1 . 0.5))
                         (par . #f))
  (make-slant-markup (- 90.0 di-wr di-wl) m)))

DiPlaneT¹ = #(define-scheme-function (m) (markup?)
 (make-override-markup `((finalrotation . ,di-wr)
                         (extrascaling . (0.5 . 1))
                         (par . #f))
  (make-slant-markup (+ di-wr di-wl -90.0) m)))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#(define roof-path `((moveto 0 0)
                     (lineto ,sz 0)
                     (lineto ,(* 0.5 sz) ,sz)
                     (closepath)))
#(define large-bow-path `((moveto 0 0)
                          (lineto ,sz 0)
                          (curveto ,sz ,(* sz bezier-round)
                                   ,(* sz (- 2 bezier-round)) ,sz
                                   ,(* sz 2) ,sz)
                          (curveto ,(* sz (+ 2 bezier-round)) ,sz
                                   ,(* sz 3) ,(* sz bezier-round)
                                   ,(* sz 3) 0)
                          (lineto ,(* sz 4) 0)
                          (lineto ,(* sz 4) ,(* sz 1.5))
                          (lineto 0 ,(* sz 1.5))
                          (closepath)))
#(define small-bow-path `((moveto 0 0)
                          (lineto ,sz 0)
                          (curveto ,sz ,(* sz bezier-round 0.5)
                                   ,(* sz 0.5 (- 3 bezier-round)) ,(* sz 0.5)
                                   ,(* sz 1.5) ,(* sz 0.5))
                          (curveto ,(* sz 0.5 (+ 3 bezier-round)) ,(* sz 0.5)
                                   ,(* sz 2) ,(* sz 0.5 bezier-round)
                                   ,(* sz 2) 0)
                          (lineto ,(* sz 3) 0)
                          (lineto ,(* sz 3) ,sz)
                          (lineto 0 ,sz)
                          (closepath)))

RoofPlane = #(define-scheme-function (r g b) (number? number? number?)
 (make-with-color-markup (list r g b)
  (make-override-markup '(filled . #t)
   (make-path-markup 0 roof-path))))
   
BowPlaneSmall = #(define-scheme-function (r g b) (number? number? number?)
 (make-with-color-markup (list r g b)
  (make-override-markup '(filled . #t)
   (make-path-markup 0 small-bow-path))))
   
BowPlaneLarge = #(define-scheme-function (r g b) (number? number? number?)
 (make-with-color-markup (list r g b)
  (make-override-markup '(filled . #t)
   (make-path-markup 0 large-bow-path))))

RectPlane = #(define-scheme-function (r g b x y) (number? number? number? number? number?)
 (make-with-color-markup (list r g b)
  (make-override-markup '(filled . #t)
   (make-path-markup 0 `((moveto 0 0)
                         (lineto ,(* sz x) 0)
                         (lineto ,(* sz x) ,(* sz y))
                         (lineto 0 ,(* sz y))
                         (closepath))))))
                         

RoofPlaneR = \markup \RoofPlane #1.0 #0.4 #0.4
RoofPlaneL = \markup \RoofPlane #0.9 #0.2 #0.2

BowPlaneLaRaw = \markup \BowPlaneLarge #0.2 #0.7 #0.2
BowPlaneLd = \markup \BowPlaneLarge #0.1 #0.5 #0.1
BowFeederL = \markup \RectPlane     #0.1 #0.5 #0.1 #1 #1
BowSideL   = \markup \RectPlane     #0.5 #0.9 #0.5 #1 #1.5
BowTopLa   = \markup \RectPlane     #0.2 #0.6 #0.2 #4 #1

BowPlaneLbRaw = \markup \BowPlaneSmall #0.7 #0.7 #0.6
BowFeederSm = \markup \RectPlane    #0.5 #0.5 #0.4 #1 #0.8
BowTopSm    = \markup \RectPlane    #0.6 #0.6 #0.5 #3 #1

RoofPlaneRV¹ = \markup \overlay {
  \RoofPlaneR
  \translate #(cons (* 0.5 sz) (* 0.4 sz))
   \scale #(cons (* 0.1 sz) (* 0.1 sz))
    \center-align \musicglyph #"clefs.F"
}

RoofPlaneLB¹ = \markup \overlay {
  \RoofPlaneL
  \translate #(cons (* 0.5 sz) (* 0.3 sz))
   \scale #(cons (* 0.1 sz) (* 0.1 sz))
    \center-align \musicglyph #"clefs.G"
}

RoofPlaneRV² = \markup \overlay {
  \RoofPlaneR
  \translate #(cons (* 0.5 sz) (* 0.1 sz))
   \scale #(cons (* 0.17 sz) (* 0.17 sz))
    \center-align \musicglyph #"pedal.Ped"
}

RoofPlaneLB² = \markup \overlay {
  \RoofPlaneL
  \translate #(cons (* 0.5 sz) (* 0.3 sz))
   \scale #(cons (* 0.1 sz) (* 0.1 sz))
    \center-align \musicglyph #"clefs.C"
}

RoofPlaneRV³ = \markup \overlay {
  \RoofPlaneR
  \translate #(cons (* 0.5 sz) (* 0.35 sz))
   \scale #(cons (* 0.22 sz) (* 0.22 sz))
    \center-align \musicglyph #"scripts.coda"
}

RoofPlaneLB³ = \markup \overlay {
  \RoofPlaneL
  \translate #(cons (* 0.5 sz) (* 0.35 sz))
   \scale #(cons (* 0.15 sz) (* 0.15 sz))
    \center-align \musicglyph #"scripts.segno"
}

LongPlane¹¹ = \markup \overlay {
  \RectPlane #0.1 #0.1 #1.0 #4 #1
  \translate #(cons (* sz 2) (* sz 0.6))
   \scale #(cons (* sz 0.15) (* sz 0.15))
    \bold \column {
      \center-align "deutschsprachiges"
      \center-align "Lilypondforum"
    }
}

LongPlane¹² = \markup \overlay {
  \RectPlane #0.3 #0.6 #1.0 #4 #1
  \translate #(cons (* sz 2) (* sz 0.4))
   \scale #(cons (* sz 0.15) (* sz 0.15))
    \center-align "https://lilypondforum.de"
}

LongPlane²¹ = \markup \overlay {
  \RectPlane #1.0 #0.5 #0.1 #2.5 #1
  \translate #(cons (* sz 1.25) (* sz 0.35))
   \scale #(cons (* sz 0.2) (* sz 0.2))
    \bold \center-align "Lilypond"
}
  
LongPlane²² = \markup \overlay {
  \RectPlane #1.0 #0.7 #0.5 #2.5 #1
  \translate #(cons (* sz 1.25) (* sz 0.4))
   \scale #(cons (* sz 0.12) (* sz 0.15))
    \center-align "www.lilypond.org"
}
  
LongPlane³¹ = \markup \overlay {
  \RectPlane #0.4 #0.2 #0.0 #2.5 #1
  \translate #(cons (* sz 1.25) (* sz 0.55))
   \scale #(cons (* sz 0.15) (* sz 0.15))
    \with-color #yellow
    \italic \override #'(baseline-skip . 2.5) \column {
      \center-align "ein Notenbild,"
      \center-align "mit dem man"
    }
}
  
LongPlane³² = \markup \overlay {
  \RectPlane #0.6 #0.3 #0.1 #2.5 #1
  \translate #(cons (* sz 1.25) (* sz 0.55))
   \scale #(cons (* sz 0.15) (* sz 0.15))
    \with-color #'(1.0 1.0 0.5)
    \italic \override #'(baseline-skip . 2.5) \column {
      \center-align "gerne vom"
      \center-align "Blatt spielt."
    }
}

BowPlaneLa = \markup \overlay {
  \BowPlaneLaRaw
  \translate #(cons (* sz 0.2) (* sz 1.2))
  %  \scale #(cons (* sz 0.15) (* sz 0.15))
    \score {
      { c'4 g' f' e'8 d' b'4. a'16 g' f'2
        e''16( d'') c''-. b'-. \acciaccatura e'8 g' a'16 b'
        \repeat tremolo 4 { c''16 a' }
        b'8 g'4 f'8( ~ f'8 d'8 c'4)_\fermata \bar "|." }
      \layout {
        ragged-right = ##f
        line-width = #(* 6.3 sz)
        indent = 0
        \context {
          \Score
          \override NonMusicalPaperColumn.line-break-permission = ##f
          \override SpacingSpanner.common-shortest-duration = #(ly:make-moment 1/1)
        }
      }
    }
  \translate #(cons (* 0.5 sz) (* 0.5 sz))
   \scale #(cons (* 0.22 sz) (* 0.22 sz))
    \center-align \sharp
  \translate #(cons (* 3.5 sz) (* 0.3 sz))
   \scale #(cons (* 0.22 sz) (* 0.22 sz))
    \center-align \flat
  
}

BowPlaneLb = \markup \overlay {
  \BowPlaneLbRaw
  \translate #(cons (* sz 0.2) (* sz 0.7))
  %  \scale #(cons (* sz 0.15) (* sz 0.15))
    \score {
      { \clef alto
        c'4. f8 g4 b d'\downbow f'2 e'8( d'
        c'8 b a g f) e4.\trill
        c1\fermata \bar "|." }
      \layout {
        ragged-right = ##f
        line-width = #(* 4.7 sz)
        indent = 0
        \context {
          \Score
          \override NonMusicalPaperColumn.line-break-permission = ##f
          \override SpacingSpanner.common-shortest-duration = #(ly:make-moment 1/1)
        }
      }
    }
  \translate #(cons (* 0.5 sz) (* 0.25 sz))
   \scale #(cons (* 0.12 sz) (* 0.12 sz))
    \center-align \natural
  \translate #(cons (* 2.5 sz) (* 0.15 sz))
   \scale #(cons (* 0.12 sz) (* 0.12 sz))
    \center-align \doubleflat
  
}

% Check, no projection
%{ 
\markup {
 \RoofPlaneRV¹ \RoofPlaneLB¹ \RoofPlaneRV² \RoofPlaneLB² \RoofPlaneRV³ \RoofPlaneLB³
}
\markup {
  \BowPlaneLa
  \BowPlaneLb
}

\markup \column {
  \line { \LongPlane¹¹ \LongPlane²¹ }
  \line { \LongPlane¹² \LongPlane²² }
  \line { \LongPlane³¹ \LongPlane³² }
}
%}

% Simple Dimetric 3D image
\markup \left-align \overlay {
  % Pilars
  \translate #(di-map sz  0 1 0) \DiPlaneL¹ \LongPlane¹¹
  \translate #(di-map sz -1 1 0) \DiPlaneR¹ \LongPlane¹²

  \translate #(di-map sz -3 4 0) \DiPlaneL¹ \LongPlane³¹
  \translate #(di-map sz -4 4 0) \DiPlaneR¹ \LongPlane³²

  \translate #(di-map sz  0 4 0) \DiPlaneL¹ \LongPlane²¹
  \translate #(di-map sz -1 4 0) \DiPlaneR¹ \LongPlane²²

  % Bow brick on the right  
  \translate #(di-map sz -3 3 2.5) \DiPlaneL \BowFeederL
  \translate #(di-map sz  0 4 2.5) \DiPlaneR \BowPlaneLa
  \translate #(di-map sz  0 3 2.5) \DiPlaneL \RectPlane #0 #0.4 #0 #1 #1.5
  \translate #(di-map sz 0 3 4) \DiPlaneT \RectPlane #0.2 #0.4 #0.1 #1 #4
  
  % Small bow brick above
  \translate #(di-map sz -3 3 4) \DiPlaneL \BowFeederSm
  \translate #(di-map sz -1 4 4) \DiPlaneR \BowPlaneLb
  \translate #(di-map sz -1 4 5) \DiPlaneT¹ \BowTopSm
  
  % Bow brick to the left
  \translate #(di-map sz -1 0 4) \DiPlaneL \BowPlaneLd
  \translate #(di-map sz  0 1 4) \DiPlaneR \BowFeederL
  \translate #(di-map sz  0 0 4) \DiPlaneL \BowPlaneLa
  \translate #(di-map sz  0 4 4) \DiPlaneR \BowSideL
  \translate #(di-map sz  0 0 5.5) \DiPlaneT \BowTopLa
  
  % Roof
  \translate #(di-map sz  0 0 5.5) \DiRoofL \RoofPlaneLB¹
  \translate #(di-map sz  0 1 5.5) \DiRoofR \RoofPlaneRV¹
  \translate #(di-map sz -3 3 5.0) \DiRoofL \RoofPlaneLB³
  \translate #(di-map sz -3 4 5.0) \DiRoofR \RoofPlaneRV³
  \translate #(di-map sz  0 3 5.5) \DiRoofL \RoofPlaneLB²
  \translate #(di-map sz  0 4 5.5) \DiRoofR \RoofPlaneRV²

  % Coordinate aid
  %{
  \translate #(di-map sz 0 0 0) \left-align ". 0 0 0"
  \translate #(di-map sz 1 0 0) \left-align ". 1 0 0"
  \translate #(di-map sz 2 0 0) \left-align ". 2 0 0"
  \translate #(di-map sz 3 0 0) \left-align ". 3 0 0"
  \translate #(di-map sz 0 1 0) \left-align ". 0 1 0"
  \translate #(di-map sz 0 2 0) \left-align ". 0 2 0"
  \translate #(di-map sz 0 3 0) \left-align ". 0 3 0"
  \translate #(di-map sz 0 4 0) \left-align ". 0 4 0"
  \translate #(di-map sz -4 4 0) \left-align ". -4 4 0"
  \translate #(di-map sz 0 0 1) \left-align ". 0 0 1"
  % \translate #(di-map sz 0 0 1.5) \left-align ". 0 0 1.5"
  % \translate #(di-map sz 0 1 1.5) \left-align ". 0 1 1.5"
  % \translate #(di-map sz -1 1 1.5) \left-align ". -1 1 1.5"
  % \translate #(di-map sz -0.5 0.5 2.5) \left-align ". -0.5 0.5 2.5"
  %}
}