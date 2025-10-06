; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/093.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.union (str.to_re "") (str.to_re "a"))))
(assert (not (=> (not (= (str.len s) 0)) (=> (not (= s "a")) false))))
(check-sat)
(exit)