; Input: /benchmark/subjects/125.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (str.to_re "a") (re.++ ((_ re.^ 0) re.allchar) (re.* re.allchar)))))
(assert (= s ""))
(assert (not false))
(check-sat)
(exit)