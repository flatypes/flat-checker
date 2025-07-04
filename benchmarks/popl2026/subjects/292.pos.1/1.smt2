; Input: /benchmark/subjects/292.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.opt (str.to_re "a")) (re.opt (str.to_re "b")))))
(assert (distinct (str.len s) 0))
(assert (= (str.indexof s "b" 0) 0))
(assert (not (= (str.len s) 1)))
(check-sat)
(exit)