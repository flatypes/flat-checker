; Input: /benchmark/subjects/214.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (str.to_re "a") (str.to_re "b"))))
(assert (not (= (str.++ (str.at s 0) (str.at s 1)) "ab")))
(check-sat)
(exit)