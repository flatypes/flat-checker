; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/015.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s re.allchar))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (and (>= 0 0) (< 0 _let_1)))) (let ((_let_3 (and _let_2 (=> _let_2 (str.in_re (str.at s 0) re.allchar))))) (let ((_let_4 (not (= _let_1 1)))) (not (and (=> _let_4 (and false _let_3)) (=> (not _let_4) _let_3))))))))
(check-sat)
(exit)