; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/067.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ re.allchar re.allchar) re.allchar)))
(assert (not (str.in_re (str.substr s 0 (- 3 0)) (re.++ (re.++ re.allchar re.allchar) re.allchar))))
(check-sat)
(exit)