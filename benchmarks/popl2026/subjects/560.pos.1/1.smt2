; Input: /benchmark/subjects/560.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ (re.opt (str.to_re "a")) (str.to_re "b")) (re.* re.allchar))))
(assert (not (str.contains s "b")))
(check-sat)
(exit)