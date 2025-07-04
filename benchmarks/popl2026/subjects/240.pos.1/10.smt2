; Input: /benchmark/subjects/240.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.* (str.to_re "a")) (re.* (str.to_re "b")))))
(assert (>= (str.indexof s "b" 0) 0))
(assert (not (and (<= 0 0) (<= 0 (str.indexof s "b" 0)))))
(check-sat)
(exit)