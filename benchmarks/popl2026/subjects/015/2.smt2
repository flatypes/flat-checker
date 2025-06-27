; Input: /benchmark/subjects/015.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s re.allchar))
(assert (distinct (str.len s) 1))
(assert (not (and (>= 0 0) (< 0 (str.len s)))))
(check-sat)
(exit)