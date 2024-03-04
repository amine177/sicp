; let Si be the symbol with the i_th ranked weight
; let N_Si the number of occurences of a symbol Si
; Given a good dispersion of the symbols
; the encoding tree will be balanced
; O(encode-symbol(Si)) = O(log(i)), i number of bits of the symbol
; O(encode) = sum(i: 1->n, N_Si * O(log(i))) close to O(n log(n))
; In the case of an unbaLanced tree
; O(encode) close to O(n^2)
; The optimal case is for any symbol Si of length li
; to satisfy : li = -log_2(weight_Si) (entropy being the lower bound
; of a symbol length sum(p_ilog(1/p_i)) = sum(p_il_i) ,
; l_i : length of the symbol s_i and p_i the probability of symbol
; s_i appearing in text)
; then weight_Si = 1/2^i
; this decreasing nature of the symbol
; weights should be satisfied even with
; natural number, therfore the weights of 2, 2^2, ... , 2^n
; converge in their order of growth to the worst case and that is
; O(n^2)
; to reach the optimal case the proabilities of symbols (or weights)
; should follow a geometric probability distribution
