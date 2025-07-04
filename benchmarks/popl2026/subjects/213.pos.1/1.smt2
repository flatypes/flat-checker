; Input: /benchmark/subjects/213.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (str.to_re "a") (str.to_re "b"))))
(assert (not (= (str.len s) 2)))
(check-sat)
(exit)