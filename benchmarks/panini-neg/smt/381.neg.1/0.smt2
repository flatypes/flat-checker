; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/381.neg.1.py
(set-logic ALL)
(declare-const s String)
(declare-const i@1 Int)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.++ (re.union (re.diff re.allchar (re.range "a" "b")) (re.union (re.++ _let_1 (re.++ (re.* _let_1) (re.diff re.allchar _let_1))) (re.++ (str.to_re "b") re.allchar))) (re.* re.allchar)))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (< i@1 _let_1))) (let ((_let_3 (>= i@1 0))) (let ((_let_4 (and _let_3 (<= i@1 _let_1)))) (let ((_let_5 (and _let_3 _let_2))) (let ((_let_6 (distinct (str.at s i@1) "a"))) (let ((_let_7 (and _let_6 _let_5))) (let ((_let_8 (+ i@1 1))) (not (and (and (>= 0 0) (<= 0 _let_1)) (and (=> _let_2 (=> _let_4 (and (and _let_5 (=> _let_7 _let_4)) (=> (and (not _let_6) _let_5) (and (>= _let_8 0) (<= _let_8 _let_1)))))) (=> (or (not _let_2) _let_7) (=> _let_4 (=> _let_2 (= s "b"))))))))))))))))
(check-sat)
(exit)