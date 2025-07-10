; Input: /benchmark/subjects/020.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.opt re.allchar)))
(assert (not (<= (str.len s) 1)))
(check-sat)
(exit)