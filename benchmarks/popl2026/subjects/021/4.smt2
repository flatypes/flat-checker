; Input: /benchmark/subjects/021.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.opt re.allchar)))
(assert (distinct (str.len s) 1))
(assert (distinct (str.len s) 0))
(assert (not false))
(check-sat)
(exit)