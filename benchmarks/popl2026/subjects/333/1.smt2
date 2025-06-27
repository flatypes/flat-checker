; Input: /benchmark/subjects/333.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s ((_ re.loop 0 1) (re.++ (re.++ (str.to_re "a") re.allchar) (str.to_re "b")))))
(assert (> (str.len s) 0))
(assert (not (= (str.len (str.substr s 0 (- 2 0))) 2)))
(check-sat)
(exit)