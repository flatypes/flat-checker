; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/281.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.union (str.to_re "") (re.++ (str.to_re "a") (str.to_re "b")))))
(assert (not (=> (> (str.len s) 0) (= s "ab"))))
(check-sat)
(exit)