; Input: /benchmark/subjects/500.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "c"))) (let ((_let_2 (str.to_re "b"))) (let ((_let_3 (str.to_re "a"))) (str.in_re s (re.++ (re.++ (re.++ ((_ re.^ 0) _let_3) (re.* _let_3)) (re.++ ((_ re.^ 0) _let_2) (re.* _let_2))) (re.++ ((_ re.^ 0) _let_1) (re.* _let_1))))))))
(assert (not (and (<= 0 0) (<= 0 (ite (str.contains s "b") (str.indexof s "b" 0) (ite (str.contains s "c") (str.indexof s "c" 0) (str.len s)))))))
(check-sat)
(exit)