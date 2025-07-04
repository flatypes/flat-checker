; Input: /benchmark/subjects/014.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.opt (re.++ (re.++ re.allchar re.allchar) (re.* re.allchar)))))
(assert (distinct (str.len s) 1))
(assert (not false))
(check-sat)
(exit)