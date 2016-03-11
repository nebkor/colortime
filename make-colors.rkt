#lang racket

(require racket/draw)

(struct hsv-color (h s v))
(struct rgb-color (r g b))
(struct color-time (h m s))

(define/contract (time-string t)
  (-> number? string?)
  (~r t #:min-width 2 #:pad-string "0"))

(define (mk-times)
  (for*/list
      ([h 24]
       [m 60]
       [s 60]) ; generator expressions
    (~a (time-string h) (time-string m) (time-string s)
        #:separator ":")))

(define (make-range-map from-min from-max to-min to-max)
  ;; stolen with minor differences from http://rosettacode.org/wiki/Map_range#Racket
  (let ([a (- from-max from-min)] [b (- to-max to-min)])
    (lambda (s) (exact->inexact (+ to-min (/ (* (- s from-min) b) a))))))

(define 24->hue (make-range-map 0 23 0 359))
(define 60->hue (make-range-map 0 59 0 359))

(define 24->sv (make-range-map 0 23 0.0 1.0))
(define 60->sv (make-range-map 0 59 0.0 1.0))

(define 24->255 (make-range-map 0 23 0 254))
(define 60->255 (make-range-map 0 59 0 254))

(define dayseconds->hue (make-range-map 0 86399 0 359))

(define (fmod p q)
  (- p (* q (truncate (/ p q)))))

(define (time->color-time ts)
  (let-values ([(h m s) (apply values (map string->number (string-split ts ":")))])
    (color-time h m s)))

(define (color-time->dayseconds ct)
  (let ([h (color-time-h ct)]
        [m (color-time-m ct)]
        [s (color-time-s ct)])
    (+ (* 3600 h) (* 60 m) s)))

(define (hsv-maker hue-select saturation-select value-select)
  (lambda (ct)
    (hsv-color (hue-select ct) (saturation-select ct) (value-select ct))))

(define (number->byte num)
  (inexact->exact (floor (round num))))

(define (color-time->rgb ct)
  (let* ([h (color-time-h ct)]
         [m (color-time-m ct)]
         [s (color-time-s ct)]
         [r (number->byte (24->255 h))]
         [g (number->byte (60->255 m))]
         [b (number->byte (60->255 s))])
    (rgb-color r g b)))

(define color-time->hsv
  ;; hour -> hue, minute -> saturation, second -> value
  (let ([hs (compose 24->hue color-time-h)]
        [ss (compose 60->sv color-time-m)]
        [vs (compose 60->sv color-time-s)])
    (hsv-maker hs ss vs)))

(define color-time->hvs
  ;; hour -> hue, minute -> value, second -> saturation
  (let ([hs (compose 24->hue color-time-h)]
        [vs (compose 60->sv color-time-m)]
        [ss (compose 60->sv color-time-s)])
    (hsv-maker hs ss vs)))

(define color-time->svh
  ;; hour -> saturation, minute -> value, second -> hue
  (let ([ss (compose 24->sv color-time-h)]
        [vs (compose 60->sv color-time-m)]
        [hs (compose 60->hue color-time-s)])
    (hsv-maker hs ss vs)))

(define color-time->shv
  ;; hour -> saturation, minute -> hue, second -> value
  (let ([ss (compose 24->sv color-time-h)]
        [hs (compose 60->hue color-time-m)]
        [vs (compose 60->sv color-time-s)])
    (hsv-maker hs ss vs)))

(define color-time->vhs
  ;; hour -> value, minute -> hue, second -> saturation
  (let ([vs (compose 24->sv color-time-h)]
        [hs (compose 60->hue color-time-m)]
        [ss (compose 60->sv color-time-s)])
    (hsv-maker hs ss vs)))

(define color-time->vsh
  ;; hour -> value, minute -> saturation, second -> hue
  (let ([vs (compose 24->sv color-time-h)]
        [ss (compose 60->sv color-time-m)]
        [hs (compose 60->hue color-time-s)])
    (hsv-maker hs ss vs)))

(define (color-time->hue ct)
  (hsv-color (dayseconds->hue (color-time->dayseconds ct)) 1 1))

(define (hsv->rgb hsv)
  ;; https://en.wikipedia.org/wiki/HSL_and_HSV#From_HSV
  (let* ([h (hsv-color-h hsv)]
         [s (hsv-color-s hsv)]
         [v (hsv-color-v hsv)]
         [c (* v s)]
         [hp (/ h 60.0)]
         [x (- c (* c (abs (sub1 (fmod hp 2)))))] ; C - (C * |H' mod2 -1|)
         [m (- v c)])
    (let-values ([(r g b)
                  (apply values
                         (map
                          (lambda (y)
                            (number->byte (* 255 (+ m y))))
                          (match `(,hp ,c ,x)
                            [(list 0 _ _) (list 0 0 0)]
                            [(list (? (lambda (x) (<= x 1))) a b) (list a b 0)]
                            [(list (? (lambda (x) (<= x 2))) a b) (list b a 0)]
                            [(list (? (lambda (x) (<= x 3))) a b) (list 0 a b)]
                            [(list (? (lambda (x) (<= x 4))) a b) (list 0 b c)]
                            [(list (? (lambda (x) (<= x 5))) a b) (list b 0 c)]
                            [(list (? (lambda (x) (<= x 6))) a b) (list a 0 b)])))])
      (rgb-color r g b))))

(define (hsv->color hsv)
  (let* ([rgb (hsv->rgb hsv)]
         [r (rgb-color-r rgb)]
         [g (rgb-color-g rgb)]
         [b (rgb-color-b rgb)])
    (make-color r g b)))

(define (rgb->color rgb)
  (let ([r (rgb-color-r rgb)]
        [g (rgb-color-g rgb)]
        [b (rgb-color-b rgb)])
    (make-color r g b)))

(define (rgb->string c)
  (~a "R:" (rgb-color-r c) " G:" (rgb-color-g c) " B:" (rgb-color-b c)))

(define (draw-color-times-linear time-colors color-width color-height target dc file-name)
  (send dc set-smoothing 'aligned)
  (for ([x (in-range (length time-colors))]
        [color (in-list time-colors)])
    (send dc set-pen color 1 'solid)
    (send dc draw-line x 0 x color-height))
  (send target save-file file-name 'png))

(define color-times
  (for/list ([i (in-list (mk-times))])
            (time->color-time i)))

(define hsv-times (map hsv->color (map color-time->hsv color-times)))
(define hvs-times (map hsv->color (map color-time->hvs color-times)))
(define svh-times (map hsv->color (map color-time->svh color-times)))
(define shv-times (map hsv->color (map color-time->shv color-times)))
(define vhs-times (map hsv->color (map color-time->vhs color-times)))
(define vsh-times (map hsv->color (map color-time->vsh color-times)))
(define hue-times (map hsv->color (map color-time->hue color-times)))
(define rgb-times (map rgb->color (map color-time->rgb color-times)))

;; For some reason, I can't make a bitmap that's 86400xN, so sample
;; the time range with this.
(define (every-nth lst n)
  (for/list ([i (in-range (length lst))]
             #:when (zero? (modulo i n)))
    (list-ref lst i)))

(define c-width 1)
(define c-height 10000)
(define nth-to-pick 4)

(define target (make-bitmap (/ (length color-times) nth-to-pick) c-height))
(define dc (send target make-dc))

;; WARNING! Will create files on disk if you evaluate this file!
;; (draw-color-times-linear (every-nth hue-times nth-to-pick) c-width c-height target dc "hue.png")
;; (draw-color-times-linear (every-nth vsh-times nth-to-pick) c-width c-height target dc "vsh.png")
;; (draw-color-times-linear (every-nth vhs-times nth-to-pick) c-width c-height target dc "vhs.png")
;; (draw-color-times-linear (every-nth shv-times nth-to-pick) c-width c-height target dc "shv.png")
;; (draw-color-times-linear (every-nth svh-times nth-to-pick) c-width c-height target dc "svh.png")
;; (draw-color-times-linear (every-nth hvs-times nth-to-pick) c-width c-height target dc "hvs.png")
;; (draw-color-times-linear (every-nth hsv-times nth-to-pick) c-width c-height target dc "hsv.png")
;; (draw-color-times-linear (every-nth rgb-times nth-to-pick) c-width c-height target dc "rgb.png")
