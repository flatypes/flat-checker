; Input: /benchmark/subjects/145.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.* (re.diff re.allchar (str.to_re "a")))))
(assert (not (or false (not (str.contains (str.substr s 0 (- 0 0)) "a")))))
(check-sat)
(exit)