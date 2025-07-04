; Input: /benchmark/subjects/010.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.opt (re.++ (re.++ re.allchar re.allchar) (re.* re.allchar)))))
(assert (not (= (str.len s) 1)))
(check-sat)
(exit)