(declare-fun loop_size () Int)
(assert (= (mod (^ 7 loop_size) 20201227) 1169255))
(check-sat)
(get-model)
