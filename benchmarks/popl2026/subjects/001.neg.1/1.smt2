; Input: /benchmark/subjects/001.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ re.allchar (re.* re.allchar))))
(assert (not (= s "")))
(check-sat)
(exit)