; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/066.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ re.allchar re.allchar) re.allchar)))
(assert (<= (str.len s) 3))
(assert (not (str.in_re (str.substr s 0 (- 3 0)) (re.++ (re.++ re.allchar re.allchar) re.allchar))))
(check-sat)
(exit)