; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/303.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (str.to_re "a") (re.++ (str.to_re "b") (re.* re.allchar)))))
(assert (not (and (str.contains s "a") (and (= (str.indexof s "a" 0) 0) (and (str.contains s "b") (= (str.indexof s "b" 0) 1))))))
(check-sat)
(exit)