; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/021.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s ((_ re.loop 0 1) re.allchar)))
(assert (distinct (str.len s) 1))
(assert (distinct (str.len s) 0))
(assert (not false))
(check-sat)
(exit)