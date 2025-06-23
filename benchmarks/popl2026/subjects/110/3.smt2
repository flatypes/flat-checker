; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/110.py
(set-logic ALL)
(set-option :timeout 3000)

(declare-const s String)
(declare-const i@1 Int)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.++ ((_ re.^ 0) _let_1) (re.* _let_1)))))
(assert (< i@1 (str.len s)))
(assert (>= i@1 0))
(assert (<= i@1 (str.len s)))
(assert (not (= (str.at s i@1) "a")))
(check-sat)
(exit)