; Input: /benchmark/subjects/333.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.opt (re.++ (re.++ (str.to_re "a") re.allchar) (str.to_re "b")))))
(assert (> (str.len s) 0))
(assert (not (and (>= 1 0) (< 1 (str.len (str.substr s 0 (- 2 0)))))))
(check-sat)
(exit)