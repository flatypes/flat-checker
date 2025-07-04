; Input: /benchmark/subjects/390.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.opt (re.union (str.to_re "a") (str.to_re "b")))))
(assert (not (or (or (= s "a") (= s "b")) (= s ""))))
(check-sat)
(exit)