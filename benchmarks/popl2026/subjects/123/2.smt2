; Input: /benchmark/subjects/123.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (str.to_re "a") (re.++ ((_ re.^ 0) re.allchar) (re.* re.allchar)))))
(assert (not (and (>= 0 0) (<= 0 (str.len s)))))
(check-sat)
(exit)