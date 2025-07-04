; Input: /benchmark/subjects/222.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.* (str.to_re "a")) (str.to_re "b"))))
(assert (not (= (str.indexof s "b" 0) (- (str.len s) 1))))
(check-sat)
(exit)