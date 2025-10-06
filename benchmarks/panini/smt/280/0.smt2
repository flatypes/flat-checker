; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/280.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.union (str.to_re "") (re.++ (str.to_re "a") (str.to_re "b")))))
(assert (not (or (= s "") (= s "ab"))))
(check-sat)
(exit)