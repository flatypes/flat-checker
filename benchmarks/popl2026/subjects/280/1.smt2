; Input: /benchmark/subjects/280.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.opt (re.++ (str.to_re "a") (str.to_re "b")))))
(assert (not (or (= s "") (= s "ab"))))
(check-sat)
(exit)