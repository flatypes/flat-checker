; Input: /benchmark/subjects/510.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ (re.opt (str.to_re "a")) (re.opt (str.to_re "b"))) (re.opt (str.to_re "c")))))
(assert (not (or (or (or (or (or (or (or (= s "abc") (= s "ab")) (= s "a")) (= s "ac")) (= s "bc")) (= s "b")) (= s "c")) (= s ""))))
(check-sat)
(exit)