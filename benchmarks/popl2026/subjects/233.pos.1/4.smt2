; Input: /benchmark/subjects/233.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (str.to_re "a") (re.* (str.to_re "b")))))
(assert (>= 0 0))
(assert (>= (str.len s) 0))
(assert (not (= (+ (str.indexof (str.substr s 0 (- (str.len s) 0)) "a" 0) 0) 0)))
(check-sat)
(exit)