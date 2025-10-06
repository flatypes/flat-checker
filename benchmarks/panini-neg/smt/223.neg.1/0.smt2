; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/223.neg.1.py
(set-logic ALL)
(declare-const s String)
(declare-const i@1 Int)
(assert (let ((_let_1 (re.range "a" "b"))) (str.in_re s (re.* (re.union (str.to_re "a") (re.union (re.++ (re.diff re.allchar _let_1) (re.* _let_1)) (re.++ (str.to_re "b") (re.++ re.allchar (re.* re.allchar)))))))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (str.substr s i@1 (- _let_1 i@1)))) (let ((_let_3 (>= i@1 0))) (let ((_let_4 (and _let_3 (>= _let_1 0)))) (let ((_let_5 (- _let_1 1))) (let ((_let_6 (and _let_3 (<= i@1 _let_5)))) (let ((_let_7 (< i@1 _let_5))) (let ((_let_8 (+ i@1 1))) (not (and (and (>= 0 0) (<= 0 _let_5)) (and (=> _let_7 (=> _let_6 (and _let_4 (and (=> _let_4 (str.contains _let_2 "a")) (and _let_4 (and (=> _let_4 (= (+ (str.indexof _let_2 "a" 0) i@1) i@1)) (and (>= _let_8 0) (<= _let_8 _let_5)))))))) (=> (not _let_7) (=> _let_6 (and _let_4 (and (=> _let_4 (str.contains _let_2 "b")) (and _let_4 (=> _let_4 (= (+ (str.indexof _let_2 "b" 0) i@1) i@1)))))))))))))))))))
(check-sat)
(exit)