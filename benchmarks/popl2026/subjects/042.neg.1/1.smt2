; Input: /benchmark/subjects/042.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.opt (re.++ re.allchar (re.opt (re.++ (re.++ re.allchar re.allchar) (re.* re.allchar)))))))
(assert (> (str.len s) 2))
(assert (not false))
(check-sat)
(exit)