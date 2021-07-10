(import-macros {: assert= : assert-items=} :test.macros)
(local B (require :bulb))
(local unpack B.unpack)

(local tests {})

(local callable-table
  (setmetatable {} {:__call (fn [])}))

(fn tests.iterable? []
  (assert= false (B.iterable? nil))
  (assert= false (B.iterable? 1))
  (assert= true (B.iterable? []))
  (assert= true (B.iterable? ""))
  (assert= true (B.iterable? #nil))
  (assert= true (B.iterable? callable-table)))

(fn tests.iter []
  (assert= [1 2 3 4]
           (icollect [x (B.iter [1 2 3 4])] x)
           "table iter")
  (assert= ["1" "2" "3" "4"]
           (icollect [x (B.iter "1234")] x)
           "string iter")
  (assert= [1 2 3 4]
           (icollect [x (B.iter (B.iter (B.iter [1 2 3 4])))] x)
           "iter can be called multiple times")
  (assert= [] (icollect [x (B.iter #nil)] x)
           "nil function")
  (assert= [] (icollect [x (B.iter "")] x)
           "empty string")
  (assert= [] (icollect [x (B.iter [])] x)
           "empty table"))

(fn tests.builtin-iters []
  (assert= [[1 :a] [2 :b] [3 :c] [4 :d]]
           (icollect [i v (B.iter-indexed [:a :b :c :d])] [i v]))
  (assert-items= [[:a 1] [:b 2] [:c 3] [:d 4]]
                 (icollect [k v (B.iter-kv {:a 1 :b 2 :c 3 :d 4})] [k v])))

(fn tests.zip-tables []
  (assert= [[1] [2] [3]]
           (icollect [x _extra (B.zip [1 2 3])] [x _extra])
           "1-arg")
  (assert= [[1 10] [2 20] [3 30]]
           (icollect [x y _extra (B.zip [1 2 3 4] [10 20 30])] [x y _extra])
           "2-arg")
  (assert= [[1 10 100] [2 20 200] [3 30 300]]
           (icollect [x y z _extra (B.zip [1 2 3 4] [10 20 30] [100 200 300])] [x y z _extra])
           "3-arg")
  (assert= [[1 10 100 1000 10000 100000 1000000]
            [2 20 200 2000 20000 200000 2000000]
            [3 30 300 3000 30000 300000 3000000]]
           (icollect [a b c d e f g _extra (B.zip [1 2 3 4] [10 20 30] [100 200 300]
                                                  [1000 2000 3000] [10000 20000 30000]
                                                  [100000 200000 300000] [1000000 2000000 3000000])]
             [a b c d e f g _extra])
           "many args")
  (let [as [1 2 3 4]
        bs [:a :b :c :d]
        cs [9 8 7 6]]
    (assert= [as bs cs]
             (->> (B.zip as bs cs)
                  (B.mapt #[$...])
                  (unpack)
                  (B.zip)
                  (B.mapt #[$...]))
             "zip twice is a no-op")))

(fn tests.zip-iters []
  (assert= [[:h] [:e] [:l] [:l] [:o]]
           (icollect [x _extra (B.zip "hello")] [x _extra])
           "1 string")
  (assert= [[:h 1] [:e 2] [:l 3]]
           (icollect [x y _extra (B.zip "hello" [1 2 3])] [x y _extra])
           "string and table")
  (assert= [[:h 1 :x] [:e 2 :x] [:l 3 :x]]
           (icollect [x y z _extra (B.zip "hello" [1 2 3] #:x)] [x y z _extra])
           "string, table, and function")
  (assert= [[1 "h" "e" "l" "l" "o"]
            [2 "e" "l" "l" "o" " "]
            [3 "l" "l" "o" " " " "]]
           (icollect [a b c d e f _extra (B.zip [1 2 3] "hello" "ello "
                                                "llo  " "lo   " "o    ")]
             [a b c d e f _extra])
           "many iterables"))


(fn tests.catv []
  (assert= 15 (->> (B.range 5) (B.map #(values $ $ $)) B.sum)
           "map retains multivals")
  (assert= 45 (->> (B.range 5) (B.map #(values $ $ $)) B.catv B.sum)
           "catv flattens multivals")
  (assert= 45 (->> (B.range 5) (B.mapcatv #(values $ $ $)) B.sum)
           "mapcatv is just (comp catv map)"))

(fn tests.mapcat []
  (assert= false (pcall #(->> (B.range 5) (B.mapcat #$) B.sum))
           "mapcat throws on numbers (expects an iterable)")
  (assert= false (pcall #(->> (B.range 5) (B.mapcat #(values $ $ $)) B.sum))
           "mapcat throws on multiple numbers (expects an iterable)")
  (assert= 45 (->> (B.range 5) (B.mapcat #[$ $ $]) B.sum)
           "mapcat flattens tables")
  (assert= 45 (->> (B.range 5) (B.mapcat #(B.take 3 (fn [] $))) B.sum)
           "mapcat flattens other iterables"))

(fn each-sum [iterable]
  (var total 0)
  (each [x iterable]
    (set total (+ total x)))
  total)

(fn tests.iter-cached []
  ;; example from the docstring
  (let [xs-uncached (B.map #(* $ $) (B.range 10))
        xs (B.iter-cached (B.map #(* $ $) (B.range 10)))]
    (assert= 385 (B.sum xs-uncached) "sum works the first time")
    (assert= 0 (B.sum xs-uncached) "uncached sum does not work the second time")
    (assert= 385 (B.sum xs) "sum works the first time")
    (assert= 385 (B.sum xs) "cached sum works the second time")
    ;; using generic for
    (assert= 385 (each-sum (xs:copy))
             "each works the first time, when copied")
    (assert= 385 (each-sum (xs:copy))
             "each works the second time, when copied")
    (assert= 0 (do (each-sum xs) (each-sum xs))
             "each does not work twice unless copied")))

(fn tests.iter-cached-table []
  (let [xs (B.iter-cached [1 2 3 4 5 6 7 8 9 10])]
    (assert= 55 (B.sum xs) "sum works the first time")
    (assert= 55 (B.sum xs) "sum works the second time")
    ;; using generic for
    (assert= 55 (each-sum (xs:copy))
             "each works the first time, when copied")
    (assert= 55 (each-sum (xs:copy))
             "each works the second time, when copied")
    (assert= 0 (do (each-sum xs) (each-sum xs))
             "each does not work twice unless copied")))

(fn tests.iter-cached-string []
  (let [xs (B.iter-cached "hello")]
    (assert= [:h :e :l :l :o] (B.totable xs) "totable works the first time")
    (assert= [:h :e :l :l :o] (B.totable xs) "totable works the second time")
    ;; using generic for
    (assert= [:h :e :l :l :o] (icollect [x (xs:copy)] x)
             "icollect works the first time, when copied")
    (assert= [:h :e :l :l :o] (icollect [x (xs:copy)] x)
             "icollect works the second time, when copied")
    (assert= [] (do (icollect [x xs] x) (icollect [x xs] x))
             "icollect does not work twice unless copied")))

(fn tests.iter-cached-values []
  (let [xs (B.iter-cached (B.map #(values $ $ $) "abc"))]
    (assert= [:a :a :a :b :b :b :c :c :c] (B.totable+ xs)
             "totable+ works the first time")
    (assert= [:a :a :a :b :b :b :c :c :c] (B.totable+ xs)
             "totable+ works the second time")
    ;; using generic for
    (assert= [[:a :a :a] [:b :b :b] [:c :c :c]] (icollect [a b c (xs:copy)] [a b c])
             "icollect works the first time, when copied")
    (assert= [[:a :a :a] [:b :b :b] [:c :c :c]] (icollect [a b c (xs:copy)] [a b c])
             "icollect works the first time, when copied")
    (assert= [] (do (icollect [a b c xs] [a b c]) (icollect [a b c xs] [a b c]))
             "icollect does not work twice unless copied")))

(fn tests.iter-cached-stops []
  ;; Without the check for `head.end?` the second and third tests fail
  (fn strict-iter [end]
    (var i 0)
    (fn []
      (set i (+ i 1))
      (if
        (<= i end) i
        (= i (+ 1 end)) nil ; end condition -- iteration should stop here
        (error "too far!"))))
  (let [xs (B.iter-cached (strict-iter 10))]
    (assert= true (pcall #(B.sum (xs:copy)))
             "First time through stops at the right place")
    (assert= true (pcall #(B.sum (xs:copy)))
             "Second time through stops at the right place")
    (assert= true (pcall #(B.sum (xs:copy)))
             "Third time through stops at the right place")))


(fn tests.partition-table []
  ;; based on clojure tests
  (assert= [[1 2]] (B.totable (B.partition 2 [1 2 3])))
  (assert= [[1 2] [3 4]] (B.totable (B.partition 2 [1 2 3 4])))
  (assert= [] (B.totable (B.partition 2 [])))

  (assert= [[1 2] [4 5]] (B.totable (B.partition 2 3 [1 2 3 4 5 6 7])))
  (assert= [[1 2] [4 5] [7 8]] (B.totable (B.partition 2 3 [1 2 3 4 5 6 7 8])))
  (assert= [] (B.totable (B.partition 2 3 [])))

  (assert= [] (B.totable (B.partition 1 [])))
  (assert= [[1] [2] [3]] (B.totable (B.partition 1 [1 2 3])))

  (assert= [] (B.totable (B.partition 5 [1 2 3])))

  (assert= [] (B.totable (B.partition -1 [1 2 3])))
  (assert= [] (B.totable (B.partition -2 [1 2 3])))

  ;; additional tests
  (assert= [[1 2 3 4] [3 4 5 6] [5 6 7 8]]
           (B.totable (B.partition 4 2 [1 2 3 4 5 6 7 8 9]))))

(fn tests.partition []
  (assert= [[1 2]] (B.totable (B.partition 2 (B.range 3))))
  (assert= [[1 2] [3 4]] (B.totable (B.partition 2 (B.range 4))))
  (assert= [] (B.totable (B.partition 2 [])))

  (assert= [[1 2] [4 5]] (B.totable (B.partition 2 3 (B.range 7))))
  (assert= [[1 2] [4 5] [7 8]] (B.totable (B.partition 2 3 (B.range 8))))
  (assert= [] (B.totable (B.partition 2 3 #nil)))

  (assert= [] (B.totable (B.partition 1 #nil)))
  (assert= [[1] [2] [3]] (B.totable (B.partition 1 (B.range 3))))

  (assert= [] (B.totable (B.partition 5 (B.range 3))))

  (assert= [] (B.totable (B.partition -1 (B.range 3))))
  (assert= [] (B.totable (B.partition -2 (B.range 3))))

  ;; additional tests
  (assert= [[1 2 3 4] [3 4 5 6] [5 6 7 8]]
           (B.totable (B.partition 4 2 (B.range 9))))
  ;; partition is done on a per-iteration basis, not a per value basis
  (assert= [[1 2 3 4] [3 4 5 6] [5 6 7 8]]
           (B.totable (B.partition 4 2 (B.map #(values $ $) [1 2 3 4 5 6 7 8 9])))))

tests
