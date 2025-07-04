; Input: /benchmark/subjects/440.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.union (str.to_re "c") (re.++ (str.to_re "a") (str.to_re "b")))))
(assert (not (or (= s "ab") (= s "c"))))
(check-sat)
(exit)