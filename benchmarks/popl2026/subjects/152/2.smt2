; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/152.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.diff re.allchar (str.to_re "a"))))
(assert (not (= (str.indexof s "a" 0) (- 1))))
(check-sat)
(exit)