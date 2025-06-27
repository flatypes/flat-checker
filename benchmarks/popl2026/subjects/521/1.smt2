; Input: /benchmark/subjects/521.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.++ (re.++ _let_1 _let_1) (re.++ ((_ re.^ 0) re.allchar) (re.* re.allchar))))))
(assert (not (and (>= 0 0) (< 0 (str.len s)))))
(check-sat)
(exit)