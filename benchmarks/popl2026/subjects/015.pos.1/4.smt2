; Input: /benchmark/subjects/015.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s re.allchar))
(assert (= (str.len s) 1))
(assert (not (and (>= 0 0) (< 0 (str.len s)))))
(check-sat)
(exit)