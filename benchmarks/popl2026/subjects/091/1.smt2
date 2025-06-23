; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/091.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s ((_ re.loop 0 1) (str.to_re "a"))))
(assert (not (or (= s "") (= s "a"))))
(check-sat)
(exit)