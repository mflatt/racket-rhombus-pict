#lang scribble/manual
@(require "racket-pict-id.rkt"
          "rhombus-pict-id.rhm"
          (for-label pict/racket-code-via-rhombus
                     pict))

@title{Racket Code as a Rhombus Pict}

@defmodule[pict/racket-code-via-rhombus]{

 The @racketmodname[pict/racket-code-via-rhombus] module provides support
 for typesetting Racket-like S-expression forms to generate potentially animated
 Rhombus @|rhombus_pict|s.

}

@defform[(code datum ...)]{

Like @|racket-code|, but creating a @|rhombus_pict| instead of a pict
satisfying @racket[pict?]. Escapes within a @racket[datum] should
produce @|rhombus_pict| values.

}

@defform[(typeset-code datum ...)]{

Like @|racket-typeset-code|, but creating a @|rhombus_pict| instead of
a pict satisfying @racket[pict?].

}
