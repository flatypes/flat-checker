; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/012.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s re.allchar))
(assert (not (str.in_re (str.at s (- (str.len s) 1)) re.allchar)))
(check-sat)
(exit)