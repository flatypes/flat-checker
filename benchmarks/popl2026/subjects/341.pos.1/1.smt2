; Input: /benchmark/subjects/341.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.* (re.++ (re.++ (str.to_re "a") re.allchar) (str.to_re "b")))))
(assert (not (and (<= 0 0) (< 0 (+ (str.len s) 3)))))
(check-sat)
(exit)