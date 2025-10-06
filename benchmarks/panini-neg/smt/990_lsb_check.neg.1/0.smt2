; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/990_lsb_check.neg.1.py
(set-logic ALL)
(declare-const s String)
(declare-const i@1 Int)
(assert (let ((_let_1 (re.range "0" "1"))) (str.in_re s (re.* (re.union (str.to_re "0") (re.union (re.++ (re.diff re.allchar _let_1) (re.* _let_1)) (re.++ (str.to_re "1") (re.++ re.allchar (re.* re.allchar)))))))))
(assert (let ((_let_1 (str.at s i@1))) (let ((_let_2 (str.len s))) (let ((_let_3 (>= i@1 0))) (let ((_let_4 (and _let_3 (< i@1 _let_2)))) (let ((_let_5 (- _let_2 1))) (let ((_let_6 (and _let_3 (<= i@1 _let_5)))) (let ((_let_7 (< i@1 _let_5))) (let ((_let_8 (+ i@1 1))) (not (and (and (>= 0 0) (<= 0 _let_5)) (and (=> _let_7 (=> _let_6 (and _let_4 (and (=> _let_4 (= _let_1 "0")) (and (>= _let_8 0) (<= _let_8 _let_5)))))) (=> (not _let_7) (=> _let_6 (and _let_4 (=> _let_4 (= _let_1 "1")))))))))))))))))
(check-sat)
(exit)