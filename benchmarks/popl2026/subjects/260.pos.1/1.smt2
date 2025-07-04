; Input: /benchmark/subjects/260.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.opt (str.to_re "a")) (str.to_re "b"))))
(assert (not (or (= s "b") (= s "ab"))))
(check-sat)
(exit)