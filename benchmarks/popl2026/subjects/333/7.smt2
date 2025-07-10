; Input: /benchmark/subjects/333.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.opt (re.++ (re.++ (str.to_re "a") re.allchar) (str.to_re "b")))))
(assert (> (str.len s) 0))
(assert (> (str.len s) 1))
(assert (not (= (str.len (str.substr s 1 (- 3 1))) 2)))
(check-sat)
(exit)