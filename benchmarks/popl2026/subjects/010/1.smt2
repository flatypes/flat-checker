; Input: /benchmark/subjects/010.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s re.allchar))
(assert (not (= (str.len s) 1)))
(check-sat)
(exit)