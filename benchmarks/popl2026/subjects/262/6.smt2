; Input: /benchmark/subjects/262.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.opt (str.to_re "a")) (str.to_re "b"))))
(assert (distinct (str.indexof s "b" 0) 1))
(assert (not (= (str.len s) 1)))
(check-sat)
(exit)