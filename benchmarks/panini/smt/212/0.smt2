; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/212.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (str.to_re "a") (str.to_re "b"))))
(assert (not (and (str.contains s "a") (and (= (str.indexof s "a" 0) 0) (and (str.contains s "b") (and (= (str.indexof s "b" 0) 1) (= (str.len s) 2)))))))
(check-sat)
(exit)