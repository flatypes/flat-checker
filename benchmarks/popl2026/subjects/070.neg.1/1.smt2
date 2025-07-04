; Input: /benchmark/subjects/070.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.opt re.allchar)))
(assert (not (>= (str.len s) 2)))
(check-sat)
(exit)