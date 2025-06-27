; Input: /benchmark/subjects/072.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ re.allchar re.allchar) (re.++ ((_ re.^ 0) re.allchar) (re.* re.allchar)))))
(assert (not (str.in_re (str.++ (str.at s 0) (str.at s 1)) (re.++ re.allchar re.allchar))))
(check-sat)
(exit)