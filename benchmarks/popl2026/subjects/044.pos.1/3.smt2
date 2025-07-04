; Input: /benchmark/subjects/044.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ re.allchar re.allchar)))
(assert (not (= (- (str.len s) 2) 0)))
(check-sat)
(exit)