; Input: /benchmark/subjects/121.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (str.to_re "a") (re.* re.allchar))))
(assert (>= 0 0))
(assert (>= 1 0))
(assert (not (= (str.substr s 0 (- 1 0)) "a")))
(check-sat)
(exit)