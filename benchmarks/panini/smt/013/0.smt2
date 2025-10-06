; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/013.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s re.allchar))
(assert (let ((_let_1 (str.len s))) (not (and (and (>= 0 0) (>= _let_1 0)) (<= (str.len (str.substr s 0 (- _let_1 0))) 1)))))
(check-sat)
(exit)