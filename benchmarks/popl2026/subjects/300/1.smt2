; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/300.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ (str.to_re "a") (str.to_re "b")) (re.++ ((_ re.^ 0) re.allchar) (re.* re.allchar)))))
(assert (not (and (>= 0 0) (< 0 (str.len s)))))
(check-sat)
(exit)