; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/002.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ re.allchar (re.* re.allchar))))
(assert (not (=> (distinct s "") false)))
(check-sat)
(exit)