; Input: /benchmark/subjects/233.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (str.to_re "a") (re.* (str.to_re "b")))))
(assert (let ((_let_1 (+ 0 1))) (not (and (<= 1 _let_1) (<= _let_1 (str.len s))))))
(check-sat)
(exit)