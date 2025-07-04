; Input: /benchmark/subjects/091.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.opt (str.to_re "a"))))
(assert (not (or (= s "") (= s "a"))))
(check-sat)
(exit)