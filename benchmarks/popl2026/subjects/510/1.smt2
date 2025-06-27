; Input: /benchmark/subjects/510.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ ((_ re.loop 0 1) (str.to_re "a")) ((_ re.loop 0 1) (str.to_re "b"))) ((_ re.loop 0 1) (str.to_re "c")))))
(assert (not (or (or (or (or (or (or (or (= s "abc") (= s "ab")) (= s "a")) (= s "ac")) (= s "bc")) (= s "b")) (= s "c")) (= s ""))))
(check-sat)
(exit)