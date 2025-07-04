; Input: /benchmark/subjects/045.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.opt (re.++ re.allchar (re.opt (re.++ (re.++ re.allchar re.allchar) (re.* re.allchar)))))))
(assert (not (and (str.in_re (str.at s 0) re.allchar) (str.in_re (str.at s 1) re.allchar))))
(check-sat)
(exit)