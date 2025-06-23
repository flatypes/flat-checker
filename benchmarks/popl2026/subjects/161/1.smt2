; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/161.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s ((_ re.loop 0 1) (re.diff re.allchar (str.to_re "a")))))
(assert (not (and (or (= s "") (distinct s "a")) (<= (str.len s) 1))))
(check-sat)
(exit)