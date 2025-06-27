; Input: /benchmark/subjects/122.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (str.to_re "a") (re.++ ((_ re.^ 0) re.allchar) (re.* re.allchar)))))
(assert (distinct (str.at s 0) "a"))
(assert (not false))
(check-sat)
(exit)