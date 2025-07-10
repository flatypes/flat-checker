; Input: /benchmark/subjects/400.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.union (re.* (str.to_re "a")) (re.* (str.to_re "b")))))
(assert (not (ite (not (str.contains s "a")) (= 0 0) true)))
(check-sat)
(exit)