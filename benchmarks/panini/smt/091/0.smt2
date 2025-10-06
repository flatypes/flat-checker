; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/091.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.union (str.to_re "") (str.to_re "a"))))
(assert (not (or (= s "") (= s "a"))))
(check-sat)
(exit)