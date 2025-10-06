; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/125.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1))))))
(assert (let ((_let_1 (and (>= 0 0) (< 0 (str.len s))))) (let ((_let_2 (= s ""))) (not (and (=> _let_2 false) (=> (not _let_2) (and _let_1 (=> _let_1 (= (str.at s 0) "a")))))))))
(check-sat)
(exit)