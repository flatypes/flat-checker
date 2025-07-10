; Input: /benchmark/subjects/261.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.opt (str.to_re "a")) (str.to_re "b"))))
(assert (distinct (str.len s) 1))
(assert (= (str.len s) 2))
(assert (not (and (>= 1 0) (< 1 (str.len s)))))
(check-sat)
(exit)