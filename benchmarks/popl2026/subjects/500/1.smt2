; Input: /benchmark/subjects/500.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ (re.* (str.to_re "a")) (re.* (str.to_re "b"))) (re.* (str.to_re "c")))))
(assert (not (and (<= 0 0) (<= 0 (ite (str.contains s "b") (str.indexof s "b" 0) (ite (str.contains s "c") (str.indexof s "c" 0) (str.len s)))))))
(check-sat)
(exit)