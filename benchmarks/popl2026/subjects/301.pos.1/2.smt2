; Input: /benchmark/subjects/301.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ (str.to_re "a") (str.to_re "b")) (re.* re.allchar))))
(assert (>= 0 0))
(assert (>= 2 0))
(assert (not (= (str.substr s 0 (- 2 0)) "ab")))
(check-sat)
(exit)