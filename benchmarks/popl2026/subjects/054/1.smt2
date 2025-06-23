; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/054.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ re.allchar (re.++ ((_ re.^ 0) re.allchar) (re.* re.allchar)))))
(assert (not (str.in_re (str.substr s 0 (- (str.len s) 0)) (re.++ re.allchar (re.++ ((_ re.^ 0) re.allchar) (re.* re.allchar))))))
(check-sat)
(exit)