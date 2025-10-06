; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/090.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.union (str.to_re "") (str.to_re "a"))))
(assert (not (=> (> (str.len s) 0) (= s "a"))))
(check-sat)
(exit)