; Input: /benchmark/subjects/013.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.opt (re.++ (re.++ re.allchar re.allchar) (re.* re.allchar)))))
(assert (not (and (>= 0 0) (>= (str.len s) 0))))
(check-sat)
(exit)