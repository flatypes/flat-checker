; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/045.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ re.allchar re.allchar)))
(assert (not (and (str.in_re (str.at s 0) re.allchar) (str.in_re (str.at s 1) re.allchar))))
(check-sat)
(exit)