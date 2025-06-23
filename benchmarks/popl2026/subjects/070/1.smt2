; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/070.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ re.allchar re.allchar) (re.++ ((_ re.^ 0) re.allchar) (re.* re.allchar)))))
(assert (not (>= (str.len s) 2)))
(check-sat)
(exit)