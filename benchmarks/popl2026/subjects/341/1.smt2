; Input: /benchmark/subjects/341.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (re.++ (re.++ (str.to_re "a") re.allchar) (str.to_re "b")))) (str.in_re s (re.++ ((_ re.^ 0) _let_1) (re.* _let_1)))))
(assert (not (and (<= 0 0) (< 0 (+ (str.len s) 3)))))
(check-sat)
(exit)