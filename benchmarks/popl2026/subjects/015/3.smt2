; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/015.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s re.allchar))
(assert (distinct (str.len s) 1))
(assert (not (str.in_re (str.at s 0) re.allchar)))
(check-sat)
(exit)