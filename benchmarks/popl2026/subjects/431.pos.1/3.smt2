; Input: /benchmark/subjects/431.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.diff re.allchar (str.to_re "b"))))
(assert (distinct (str.at s 0) "a"))
(assert (distinct (str.at s 0) "c"))
(assert (not (distinct (str.at s 0) "b")))
(check-sat)
(exit)