; Input: /benchmark/subjects/001.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (str.to_re "")))
(assert (not (= s "")))
(check-sat)
(exit)