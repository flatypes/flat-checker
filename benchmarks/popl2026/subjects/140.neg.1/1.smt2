; Input: /benchmark/subjects/140.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.* (re.diff re.allchar (str.to_re "a")))))
(assert (not (str.contains s "a")))
(check-sat)
(exit)