; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/122.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (str.to_re "a") (re.* re.allchar))))
(assert (let ((_let_1 (and (>= 0 0) (< 0 (str.len s))))) (not (and _let_1 (=> (and (not (= (str.at s 0) "a")) _let_1) false)))))
(check-sat)
(exit)