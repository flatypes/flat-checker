; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/161.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.union (str.to_re "") (re.diff re.allchar (str.to_re "a")))))
(assert (not (and (or (= s "") (distinct s "a")) (<= (str.len s) 1))))
(check-sat)
(exit)