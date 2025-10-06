; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/055.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ re.allchar (re.* re.allchar))))
(assert (let ((_let_1 (str.at s 0))) (let ((_let_2 (and (>= 0 0) (< 0 (str.len s))))) (let ((_let_3 (= _let_1 "a"))) (not (and (and _let_2 (=> (and _let_3 _let_2) (str.in_re "A" re.allchar))) (=> (and (not _let_3) _let_2) (and _let_2 (=> _let_2 (str.in_re _let_1 re.allchar))))))))))
(check-sat)
(exit)