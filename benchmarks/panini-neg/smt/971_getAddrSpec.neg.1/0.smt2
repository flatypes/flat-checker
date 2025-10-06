; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/971_getAddrSpec.neg.1.py
(set-logic ALL)
(declare-const email String)
(assert (let ((_let_1 (str.to_re ">"))) (let ((_let_2 (str.to_re "<"))) (let ((_let_3 (re.* (re.diff re.allchar (re.union _let_2 _let_1))))) (str.in_re email (re.++ _let_3 (re.union (re.* (re.++ _let_2 _let_3)) (re.* (re.++ _let_1 (re.* (re.diff re.allchar _let_1)))))))))))
(assert (let ((_let_1 (+ (str.indexof email "<" 0) 1))) (let ((_let_2 (str.len email))) (let ((_let_3 (str.substr email _let_1 (- _let_2 _let_1)))) (let ((_let_4 (+ (str.indexof _let_3 ">" 0) _let_1))) (let ((_let_5 (>= _let_1 0))) (let ((_let_6 (and _let_5 (>= _let_4 0)))) (let ((_let_7 (and _let_5 (>= _let_2 0)))) (not (and (str.contains email "<") (and _let_7 (and (=> _let_7 (str.contains _let_3 ">")) (and _let_7 (and _let_6 (=> _let_6 (str.in_re (str.substr email _let_1 (- _let_4 _let_1)) (re.* (re.diff re.allchar (str.to_re ">")))))))))))))))))))
(check-sat)
(exit)