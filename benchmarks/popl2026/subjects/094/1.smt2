; Input: /benchmark/subjects/094.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s ((_ re.loop 0 1) (str.to_re "a"))))
(assert (> (str.len s) 0))
(assert (not (= (str.substr s 0 (- 1 0)) "a")))
(check-sat)
(exit)