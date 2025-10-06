; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/422.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.range "a" "c")))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (= _let_1 1))) (let ((_let_3 (str.at s 0))) (let ((_let_4 (distinct _let_3 "a"))) (let ((_let_5 (distinct _let_3 "b"))) (let ((_let_6 (distinct _let_3 "c"))) (not (and (and (>= 0 0) (< 0 _let_1)) (and (=> _let_4 (and (=> _let_5 (and (=> _let_6 (and false _let_2)) (=> (not _let_6) _let_2))) (=> (not _let_5) _let_2))) (=> (not _let_4) _let_2)))))))))))
(check-sat)
(exit)