; Consider a generic procedure OP where it is applied
; to N types TYPE_1 ... TYPE_N . Consider TYPE_1 to be
; restrictive in relation to TYPE_2 .. TYPE_N. 
; In that sense (OP TYPE_1 TYPE_2 ... TYPE_N) =/=
; (OP TYPE_1 (OP TYPE_2 ... TYPE_N))
; We need an apply-generic that preserves the associativity
; of OP over all arguments.
