; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/980_parser.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.union _let_1 (re.++ (re.diff re.allchar _let_1) (re.++ (str.to_re "b") (re.* re.allchar)))))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (and (>= 1 0) (< 1 _let_1)))) (let ((_let_3 (and (>= 0 0) (< 0 _let_1)))) (let ((_let_4 (= (str.at s 0) "a"))) (not (and (and _let_3 (=> (and _let_4 _let_3) (= _let_1 1))) (=> (and (not _let_4) _let_3) (and _let_2 (=> _let_2 (= (str.at s 1) "b")))))))))))
(check-sat)
(exit)