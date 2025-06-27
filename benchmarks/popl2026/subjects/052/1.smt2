; Input: /benchmark/subjects/052.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ re.allchar (re.++ ((_ re.^ 0) re.allchar) (re.* re.allchar)))))
(assert (not (>= (str.len s) 1)))
(check-sat)
(exit)