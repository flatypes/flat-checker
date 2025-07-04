; Input: /benchmark/subjects/143.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.* (re.diff re.allchar (str.to_re "a")))))
(assert (let ((_let_1 (str.len s))) (not (and (<= 0 _let_1) (<= _let_1 _let_1)))))
(check-sat)
(exit)