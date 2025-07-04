; Input: /benchmark/subjects/212.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (str.to_re "a") (str.to_re "b"))))
(assert (not (= (str.indexof s "a" 0) 0)))
(check-sat)
(exit)