; Input: /benchmark/subjects/303.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ (str.to_re "a") (str.to_re "b")) (re.* re.allchar))))
(assert (not (= (str.indexof s "a" 0) 0)))
(check-sat)
(exit)