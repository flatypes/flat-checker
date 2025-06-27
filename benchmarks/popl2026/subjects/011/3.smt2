; Input: /benchmark/subjects/011.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s re.allchar))
(assert (not (str.in_re (str.at s 0) re.allchar)))
(check-sat)
(exit)