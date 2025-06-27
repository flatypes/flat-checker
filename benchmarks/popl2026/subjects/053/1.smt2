; Input: /benchmark/subjects/053.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ re.allchar (re.++ ((_ re.^ 0) re.allchar) (re.* re.allchar)))))
(assert (= (str.len s) 1))
(assert (not (str.in_re "" (re.union (str.to_re "") re.allchar))))
(check-sat)
(exit)