; Input: /benchmark/subjects/068.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.opt (re.++ re.allchar (re.opt (re.++ re.allchar (re.opt (re.++ (re.++ re.allchar re.allchar) (re.* re.allchar)))))))))
(assert (not (= (str.len s) 3)))
(check-sat)
(exit)