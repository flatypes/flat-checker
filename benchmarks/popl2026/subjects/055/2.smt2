; Input: /benchmark/subjects/055.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ re.allchar (re.++ ((_ re.^ 0) re.allchar) (re.* re.allchar)))))
(assert (= (str.at s 0) "a"))
(assert (not (str.in_re "A" re.allchar)))
(check-sat)
(exit)