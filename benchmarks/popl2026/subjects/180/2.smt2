; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/180.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.++ (re.diff re.allchar _let_1) _let_1))))
(assert (not (distinct (str.at s 0) "a")))
(check-sat)
(exit)