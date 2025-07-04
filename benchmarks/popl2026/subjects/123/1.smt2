; Input: /benchmark/subjects/123.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (str.to_re "a") (re.* re.allchar))))
(assert (not (> (str.len s) 0)))
(check-sat)
(exit)