; Input: /benchmark/subjects/002.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ re.allchar (re.* re.allchar))))
(assert (distinct s ""))
(assert (not false))
(check-sat)
(exit)