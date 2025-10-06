; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/372.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.range "a" "b")))
(assert (not (=> (not (= s "a")) (=> (not (= s "b")) false))))
(check-sat)
(exit)