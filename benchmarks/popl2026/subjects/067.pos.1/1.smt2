; Input: /benchmark/subjects/067.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ re.allchar re.allchar) re.allchar)))
(assert (not (<= (str.len s) 3)))
(check-sat)
(exit)