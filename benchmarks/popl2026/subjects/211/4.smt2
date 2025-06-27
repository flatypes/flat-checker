; Input: /benchmark/subjects/211.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (str.to_re "a") (str.to_re "b"))))
(assert (not (= (str.at s 1) "b")))
(check-sat)
(exit)