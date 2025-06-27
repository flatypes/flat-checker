; Input: /benchmark/subjects/090.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s ((_ re.loop 0 1) (str.to_re "a"))))
(assert (> (str.len s) 0))
(assert (not (= s "a")))
(check-sat)
(exit)