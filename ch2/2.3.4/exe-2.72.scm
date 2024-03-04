; let Si be the symbol with the i_th ranked weight
; let N_Si the number of occurences of a symbol Si
; Given a good dispersion of the symbols
; the encoding tree will be balanced
; O(encode-symbol(Si)) = O(log(i)), i number of bits of the symbol
; O(encode) = sum(i: 1->n, N_Si * O(log(i))) close to O(n log(n))
; In the case of an unblanced tree
; O(encode) close to O(n^2)
; The optimal case is for any symbole Si of length li
; to satisfy : li = -log_2(1/weight_Si)
; then weight_Si = 1/2^i
; this decreasing nature of the symbol
; weights should be satisfied even with
; natural number, therfore the weights of 2, 2^2, ... , 2^n
; converge in their order of growth to the worst case and that is
; O(n^2)
