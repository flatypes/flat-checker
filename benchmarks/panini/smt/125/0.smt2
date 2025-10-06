; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/125.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (str.to_re "a") (re.* re.allchar))))
(assert (let ((_let_1 (and (>= 0 0) (< 0 (str.len s))))) (let ((_let_2 (= s ""))) (not (and (=> _let_2 false) (=> (not _let_2) (and _let_1 (=> _let_1 (= (str.at s 0) "a")))))))))
(check-sat)
(exit)