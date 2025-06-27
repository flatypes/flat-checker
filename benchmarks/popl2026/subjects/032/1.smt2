; Input: /benchmark/subjects/032.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ ((_ re.^ 0) re.allchar) (re.* re.allchar))))
(assert (= (str.len s) 0))
(assert (not (str.in_re "a" re.allchar)))
(check-sat)
(exit)