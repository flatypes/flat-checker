; Input: /benchmark/subjects/272.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (str.to_re "a") (re.opt (str.to_re "b")))))
(assert (distinct (str.indexof s "b" 0) 1))
(assert (not (= s "a")))
(check-sat)
(exit)