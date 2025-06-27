; Input: /benchmark/subjects/068.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ re.allchar re.allchar) re.allchar)))
(assert (not (= (str.len s) 3)))
(check-sat)
(exit)