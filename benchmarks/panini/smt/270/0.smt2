; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/270.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (str.to_re "a") (re.union (str.to_re "") (str.to_re "b")))))
(assert (not (or (= s "a") (= s "ab"))))
(check-sat)
(exit)