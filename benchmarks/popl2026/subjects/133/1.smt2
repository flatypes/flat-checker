; Input: /benchmark/subjects/133.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.* re.allchar) (str.to_re "a"))))
(assert (not (and (>= 0 0) (<= 0 (- (str.len s) 1)))))
(check-sat)
(exit)