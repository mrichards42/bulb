(import-macros {: assert= : is} :test.macros)
(local B (require :bulb))

(local tests {})

(local callable-table
  (setmetatable {} {:__call (fn [])}))

(fn tests.table? []
  (is (B.table? {}))
  (is (B.table? callable-table))
  (is (not (B.table? "")))
  (is (not (B.table? 1)))
  (is (not (B.table? (fn []))))
  (is (not (B.table? nil)))
  (is (not (B.table? true)))
  (is (not (B.table? false))))

(fn tests.string? []
  (is (not (B.string? {})))
  (is (not (B.string? callable-table)))
  (is (B.string? ""))
  (is (not (B.string? 1)))
  (is (not (B.string? (fn []))))
  (is (not (B.string? nil)))
  (is (not (B.string? true)))
  (is (not (B.string? false))))

(fn tests.number? []
  (is (not (B.number? {})))
  (is (not (B.number? callable-table)))
  (is (not (B.number? "")))
  (is (B.number? 1))
  (is (not (B.number? (fn []))))
  (is (not (B.number? nil)))
  (is (not (B.number? true)))
  (is (not (B.number? false))))

(fn tests.function? []
  (is (not (B.function? {})))
  (is (not (B.function? callable-table)))
  (is (not (B.function? "")))
  (is (not (B.function? 1)))
  (is (B.function? (fn [])))
  (is (not (B.function? nil)))
  (is (not (B.function? true)))
  (is (not (B.function? false))))

(fn tests.nil? []
  (is (not (B.nil? {})))
  (is (not (B.nil? callable-table)))
  (is (not (B.nil? "")))
  (is (not (B.nil? 1)))
  (is (not (B.nil? (fn []))))
  (is (B.nil? nil))
  (is (not (B.nil? true)))
  (is (not (B.nil? false))))

(fn tests.boolean? []
  (is (not (B.boolean? {})))
  (is (not (B.boolean? callable-table)))
  (is (not (B.boolean? "")))
  (is (not (B.boolean? 1)))
  (is (not (B.boolean? (fn []))))
  (is (not (B.boolean? nil)))
  (is (B.boolean? true))
  (is (B.boolean? false)))

(fn tests.true? []
  (is (not (B.true? {})))
  (is (not (B.true? callable-table)))
  (is (not (B.true? "")))
  (is (not (B.true? 1)))
  (is (not (B.true? (fn []))))
  (is (not (B.true? nil)))
  (is (B.true? true))
  (is (not (B.true? false))))

(fn tests.false? []
  (is (not (B.false? {})))
  (is (not (B.false? callable-table)))
  (is (not (B.false? "")))
  (is (not (B.false? 1)))
  (is (not (B.false? (fn []))))
  (is (not (B.false? nil)))
  (is (not (B.false? true)))
  (is (B.false? false)))

(fn tests.callable? []
  (is (not (B.callable? {})))
  (is (B.callable? callable-table))
  (is (not (B.callable? "")))
  (is (not (B.callable? 1)))
  (is (B.callable? (fn [])))
  (is (not (B.callable? nil)))
  (is (not (B.callable? true)))
  (is (not (B.callable? false))))

(fn tests.boolean []
  (assert= true (B.boolean {}))
  (assert= true (B.boolean callable-table))
  (assert= true (B.boolean ""))
  (assert= true (B.boolean 1))
  (assert= true (B.boolean 0))
  (assert= true (B.boolean (fn [])))
  (assert= false (B.boolean nil))
  (assert= true (B.boolean true))
  (assert= false (B.boolean false)))

(fn tests.empty? []
  (is (B.empty? {}))
  (is (B.empty? callable-table))
  (is (B.empty? ""))
  (is (not (B.empty? [""])))
  (is (not (B.empty? "1")))
  (is (not (B.empty? 1)))
  (is (not (B.empty? 0)))
  (is (not (B.empty? (fn []))))
  (is (not (B.empty? nil)))
  (is (not (B.empty? true)))
  (is (not (B.empty? false))))

(fn tests.not-empty []
  (assert= nil (B.not-empty []))
  (assert= nil (B.not-empty callable-table))
  (assert= nil (B.not-empty ""))
  (assert= [""] (B.not-empty [""]))
  (assert= "1" (B.not-empty "1")))

(fn tests.array? []
  (is (B.array? []) "empty is considered an array by default")
  (is (B.array? [1 2 3]))
  (is (not (B.array? {:a 1})))
  (is (not (B.array? "")))
  (is (not (B.array? callable-table))))

(fn tests.hash? []
  (is (not (B.hash? {})) "empty is _not_ considered a hash by default")
  (is (not (B.hash? [1 2 3])))
  (is (B.hash? {:a 1}))
  (is (not (B.hash? "")))
  (is (not (B.hash? callable-table))))

(fn tests.hash-or-empty? []
  (is (B.hash-or-empty? {}))
  (is (not (B.hash-or-empty? [1 2 3])))
  (is (B.hash-or-empty? {:a 1}))
  (is (not (B.hash-or-empty? "")))
  (is (not (B.hash-or-empty? callable-table))))

(fn tests.int? []
  (is (not (B.int? [1])))
  (is (not (B.int? "1")))
  (is (not (B.int? 1.5)))
  (is (B.int? 1.0))
  (is (B.int? -1.0))
  (is (B.int? 123456789))
  (is (not (B.int? 1.00000000001))))

(fn tests.float? []
  (is (not (B.float? [1])))
  (is (not (B.float? "1")))
  (is (B.float? 1.5))
  (is (not (B.float? 1.0)))
  (is (not (B.float? -1.0)))
  (is (not (B.float? 123456789)))
  (is (B.float? 1.00000000001)))

(fn tests.zero? []
  (is (not (B.zero? [1])))
  (is (not (B.zero? "1")))
  (is (B.zero? 0))
  (is (B.zero? (- 0)))
  (is (B.zero? 0.00000000000))
  (is (not (B.zero? 0.00000000001))))

(fn tests.pos? []
  (is (not (pcall #(B.pos? [1]))))
  (is (not (pcall #(B.pos? "1"))))
  (is (not (B.pos? 0)))
  (is (not (B.pos? (- 0))))
  (is (B.pos? 1))
  (is (B.pos? 20))
  (is (B.pos? 0.00000000001))
  (is (not (B.pos? -0.00000000001))))

(fn tests.neg? []
  (is (not (pcall #(B.neg? [1]))))
  (is (not (pcall #(B.neg? "1"))))
  (is (not (B.neg? 0)))
  (is (not (B.neg? (- 0))))
  (is (B.neg? -1))
  (is (B.neg? -20))
  (is (B.neg? -0.00000000001))
  (is (not (B.neg? 0.00000000001))))

(fn tests.even? []
  (is (not (pcall #(B.even? [1]))))
  (is (B.even? "2")) ; apparently % coerces strings to numbers
  (is (B.even? 0))
  (is (B.even? (- 0)))
  (is (not (B.even? -1)))
  (is (B.even? -22))
  (is (not (B.even? 21)))
  (is (not (B.even? 1.5))))

(fn tests.odd? []
  (is (not (pcall #(B.odd? [1]))))
  (is (B.odd? "1")) ; apparently % coerces strings to numbers
  (is (not (B.odd? 0)))
  (is (not (B.odd? (- 0))))
  (is (B.odd? -1))
  (is (not (B.odd? -22)))
  (is (B.odd? 21))
  (is (not (B.odd? 1.5))))

(fn tests.deep= []
  ;; different types
  (is (not (B.deep= 0 false)))
  (is (not (B.deep= 1 true)))
  (is (not (B.deep= false nil)))
  (is (not (B.deep= {} nil)))
  (is (not (B.deep= {} "")))
  ;; same type, not equal
  (is (not (B.deep= 1 0)))
  (is (not (B.deep= 1.1 1.2)))
  (is (not (B.deep= true false)))
  (is (not (B.deep= "" "nope")))
  ;; simple values
  (is (B.deep= 1 1))
  (is (B.deep= 1.1 1.1))
  (is (B.deep= true true))
  (is (B.deep= false false))
  (is (B.deep= nil nil))
  (is (B.deep= "" ""))
  ;; tables
  (let [t {}] (is (B.deep= t t) "identity"))
  (is (B.deep= {} {}))
  (is (B.deep= [1 2 3] [1 2 3]))
  (is (B.deep= [[1] [2] [3] []] [[1] [2] [3] []]))
  (is (B.deep= {:a 1 :b [2 3]} {:a 1 :b [2 3]}))
  (is (B.deep= {:a {:b {:c {:d [1 2 3]}}}} {:a {:b {:c {:d [1 2 3]}}}}))
  ;; tables, not equal
  (is (not (B.deep= [1] [])))
  (is (not (B.deep= {:a 1} {})))
  (is (not (B.deep= {:a {:b {:c {:d [1]}}}} {:a {:b {:c {:d []}}}})))
  (is (not (B.deep= {[1] 1 [2] 2 [3] 3} {[1] 1 [2] 2 [3] 3}))
      "table keys are compared by identity, not value")
  ;; tables with cycles
  (let [x [1 2 3] y [1 2 3]]
    (table.insert x x)
    (table.insert y y)
    (is (B.deep= x y)
        "tables with cycles"))
  (let [x [1 2 3 4] y [1 2 3]]
    (table.insert x x)
    (table.insert y y)
    (is (not (B.deep= x y))
        "unequal tables with cycles"))
  (let [x [1 2 3 {:a {:b {}}}] y [1 2 3 {:a {:b {}}}]]
    (tset x 4 :a :b :c x)
    (tset y 4 :a :b :c y)
    (is (B.deep= x y)
        "tables with nested cycles"))
  (let [x [1 2 3] y [1 2 3]]
    (tset x x x)
    (tset y y y)
    (is (not (B.deep= x y))
        "deep= requires keys to be identical")))

tests
