; Input: /benchmark/subjects/143.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.* (re.diff re.allchar (str.to_re "a")))))
(assert (let ((_let_1 (str.len s))) (not (not (str.contains (str.substr s _let_1 (- _let_1 _let_1)) "a")))))
(check-sat)
(exit)