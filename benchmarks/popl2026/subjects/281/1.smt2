; Input: /benchmark/subjects/281.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.opt (re.++ (str.to_re "a") (str.to_re "b")))))
(assert (> (str.len s) 0))
(assert (not (= s "ab")))
(check-sat)
(exit)