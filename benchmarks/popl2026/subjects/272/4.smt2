; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/272.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (str.to_re "a") ((_ re.loop 0 1) (str.to_re "b")))))
(assert (distinct (str.indexof s "b" 0) 1))
(assert (not (= s "a")))
(check-sat)
(exit)