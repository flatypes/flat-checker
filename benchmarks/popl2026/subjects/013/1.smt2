; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/013.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s re.allchar))
(assert (not (<= (str.len (str.substr s 0 (- (str.len s) 0))) 1)))
(check-sat)
(exit)