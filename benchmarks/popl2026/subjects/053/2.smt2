; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/053.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ re.allchar (re.++ ((_ re.^ 0) re.allchar) (re.* re.allchar)))))
(assert (distinct (str.len s) 1))
(assert (not (and (>= 0 0) (< 0 (str.len s)))))
(check-sat)
(exit)