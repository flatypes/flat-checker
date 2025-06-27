; Input: /benchmark/subjects/980_parser.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.union _let_1 (re.++ (re.++ (re.diff re.allchar _let_1) (str.to_re "b")) (re.++ ((_ re.^ 0) re.allchar) (re.* re.allchar)))))))
(assert (distinct (str.at s 0) "a"))
(assert (not (and (>= 1 0) (< 1 (str.len s)))))
(check-sat)
(exit)