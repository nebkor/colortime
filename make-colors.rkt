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

(define (fmod p q)
  (- p (* q (truncate (/ p q)))))

(define (time->color-time ts)
  (let-values ([(h m s) (apply values (map string->number (string-split ts ":")))])
    (color-time h m s)))

(define (hsv-maker hue-select saturation-select value-select)
  (lambda (ct)
    (hsv-color (hue-select ct) (saturation-select ct) (value-select ct))))

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
                            (inexact->exact (floor (round (* 255 (+ m y))))))
                          (match `(,hp ,c ,x)
                            [(list 0 _ _) (list 0 0 0)]
                            [(list (? (lambda (x) (<= x 1))) a b) (list a b 0)]
                            [(list (? (lambda (x) (<= x 2))) a b) (list b a 0)]
                            [(list (? (lambda (x) (<= x 3))) a b) (list 0 a b)]
                            [(list (? (lambda (x) (<= x 4))) a b) (list 0 b c)]
                            [(list (? (lambda (x) (<= x 5))) a b) (list b 0 c)]
                            [(list (? (lambda (x) (<= x 6))) a b) (list a 0 b)])))])
      (rgb-color r g b))))

(define (rgb->string c)
  (~a "R:" (rgb-color-r c) " G:" (rgb-color-g c) " B:" (rgb-color-b c)))
