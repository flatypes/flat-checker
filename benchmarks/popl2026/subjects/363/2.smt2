; Input: /benchmark/subjects/363.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ (str.to_re "a") (re.++ ((_ re.^ 0) re.allchar) (re.* re.allchar))) (str.to_re "b"))))
(assert (not (= (str.indexof s "a" 0) 0)))
(check-sat)
(exit)