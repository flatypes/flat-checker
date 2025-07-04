; Input: /benchmark/subjects/341.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.* (re.++ (re.++ (str.to_re "a") re.allchar) (str.to_re "b")))))
(assert (not (str.in_re (str.substr s 0 (- (str.len s) 0)) (re.* (re.++ (re.++ (str.to_re "a") re.allchar) (str.to_re "b"))))))
(check-sat)
(exit)