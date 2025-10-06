; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/120.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (str.to_re "a") (re.* re.allchar))))
(assert (let ((_let_1 (and (>= 0 0) (< 0 (str.len s))))) (not (and _let_1 (=> _let_1 (= (str.at s 0) "a"))))))
(check-sat)
(exit)