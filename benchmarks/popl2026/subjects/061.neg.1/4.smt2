; Input: /benchmark/subjects/061.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.opt (re.++ re.allchar (re.opt (re.++ re.allchar (re.opt (re.++ (re.++ re.allchar re.allchar) (re.* re.allchar)))))))))
(assert (not (and (>= 2 0) (< 2 (str.len s)))))
(check-sat)
(exit)