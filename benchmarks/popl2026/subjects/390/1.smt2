; Input: /benchmark/subjects/390.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s ((_ re.loop 0 1) (re.union (str.to_re "a") (str.to_re "b")))))
(assert (not (or (or (= s "a") (= s "b")) (= s ""))))
(check-sat)
(exit)