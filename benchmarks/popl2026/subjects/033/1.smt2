; Input: /benchmark/subjects/033.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ ((_ re.^ 0) re.allchar) (re.* re.allchar))))
(assert (not (and (>= 0 0) (<= 0 (str.len s)))))
(check-sat)
(exit)