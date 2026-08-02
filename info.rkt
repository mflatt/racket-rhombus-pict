#lang info

(define collection 'multi)

(define deps
  '("base"
    "pict-lib"
    "draw-lib"
    "rhombus-lib"
    "rhombus-pict-lib"))

(define build-deps
  '("pict"
    "rhombus-pict"
    "racket-doc"
    "scribble-lib"))

(define pkg-desc "Racket code rending as Rhimbus picts")

(define license '(Apache-2.0 OR MIT))

(define version "1.0")
