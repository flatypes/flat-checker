; Input: /benchmark/subjects/990_lsb_check.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "0"))) (str.in_re s (re.++ (re.++ ((_ re.^ 0) _let_1) (re.* _let_1)) (str.to_re "1")))))
(assert (not (and (>= 0 0) (<= 0 (- (str.len s) 1)))))
(check-sat)
(exit)