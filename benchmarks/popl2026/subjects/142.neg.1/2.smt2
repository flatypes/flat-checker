; Input: /benchmark/subjects/142.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.* (re.diff re.allchar (str.to_re "a")))))
(assert (not (not (str.contains (str.substr s 0 (- 0 0)) "a"))))
(check-sat)
(exit)