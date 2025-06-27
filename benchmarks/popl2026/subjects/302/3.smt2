; Input: /benchmark/subjects/302.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ (str.to_re "a") (str.to_re "b")) (re.++ ((_ re.^ 0) re.allchar) (re.* re.allchar)))))
(assert (not (and (>= 1 0) (< 1 (str.len (str.substr s 0 (- 2 0)))))))
(check-sat)
(exit)