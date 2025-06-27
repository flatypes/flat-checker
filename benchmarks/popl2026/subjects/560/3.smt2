; Input: /benchmark/subjects/560.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ ((_ re.loop 0 1) (str.to_re "a")) (str.to_re "b")) (re.++ ((_ re.^ 0) re.allchar) (re.* re.allchar)))))
(assert (= (str.indexof s "b" 0) 1))
(assert (not (= (str.at s 0) "a")))
(check-sat)
(exit)