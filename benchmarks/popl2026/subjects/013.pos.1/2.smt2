; Input: /benchmark/subjects/013.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s re.allchar))
(assert (not (<= (str.len (str.substr s 0 (- (str.len s) 0))) 1)))
(check-sat)
(exit)