; Input: /benchmark/subjects/212.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (str.to_re "a") (str.to_re "b"))))
(assert (not (str.contains s "a")))
(check-sat)
(exit)