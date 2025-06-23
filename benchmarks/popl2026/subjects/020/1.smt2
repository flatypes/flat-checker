; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/020.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s ((_ re.loop 0 1) re.allchar)))
(assert (not (<= (str.len s) 1)))
(check-sat)
(exit)