; Input: /benchmark/subjects/031.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ ((_ re.^ 0) re.allchar) (re.* re.allchar))))
(assert (not (>= (str.len s) 0)))
(check-sat)
(exit)