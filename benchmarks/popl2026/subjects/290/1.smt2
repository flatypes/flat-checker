; Input: /benchmark/subjects/290.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.opt (str.to_re "a")) (re.opt (str.to_re "b")))))
(assert (not (or (or (or (= s "") (= s "a")) (= s "b")) (= s "ab"))))
(check-sat)
(exit)