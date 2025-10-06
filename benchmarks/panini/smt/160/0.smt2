; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/160.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.union (str.to_re "") (re.diff re.allchar (str.to_re "a")))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (= _let_1 1))) (let ((_let_3 (and (>= 0 0) (< 0 _let_1)))) (not (and (=> _let_2 (and _let_3 (=> _let_3 (distinct (str.at s 0) "a")))) (=> (not _let_2) (= _let_1 0))))))))
(check-sat)
(exit)