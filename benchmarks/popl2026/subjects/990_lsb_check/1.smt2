; Input: /benchmark/subjects/990_lsb_check.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.* (str.to_re "0")) (str.to_re "1"))))
(assert (not (and (>= 0 0) (<= 0 (- (str.len s) 1)))))
(check-sat)
(exit)