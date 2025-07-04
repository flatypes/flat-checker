; Input: /benchmark/subjects/430.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.diff re.allchar (str.to_re "b"))))
(assert (not (or (or (= s "a") (and (distinct s "b") (= (str.len s) 1))) (= s "c"))))
(check-sat)
(exit)