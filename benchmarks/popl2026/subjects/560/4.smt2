; Input: /benchmark/subjects/560.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ (re.opt (str.to_re "a")) (str.to_re "b")) (re.* re.allchar))))
(assert (distinct (str.indexof s "b" 0) 1))
(assert (not (= (str.indexof s "b" 0) 0)))
(check-sat)
(exit)