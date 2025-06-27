; Input: /benchmark/subjects/071.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ re.allchar re.allchar) (re.++ ((_ re.^ 0) re.allchar) (re.* re.allchar)))))
(assert (not (str.in_re (str.substr s 1 (- (str.len s) 1)) (re.++ re.allchar (re.++ ((_ re.^ 0) re.allchar) (re.* re.allchar))))))
(check-sat)
(exit)