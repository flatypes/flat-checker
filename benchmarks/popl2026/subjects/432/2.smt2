; Input: /benchmark/subjects/432.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.diff re.allchar (str.to_re "b"))))
(assert (= (str.at s 0) "b"))
(assert (not false))
(check-sat)
(exit)