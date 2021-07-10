(import-macros {: assert= : assert-items= : assert-not= : is} :test.macros)
(local B (require :bulb))

(local tests {})

;;; array tables

(fn tests.conj! []
  (assert= [0] (B.conj! [0]) "adding no items")
  (assert= [0 :a] (B.conj! [0] :a) "adding one item")
  (assert= [0 :a :b :c :d] (B.conj! [0] :a :b :c :d) "adding several items")
  (assert= [0 :a :b :c :d] (B.conj! [0] :a nil :b nil :c nil :d nil nil)
           "nils are skipped"))

(fn tests.repeat []
  (assert= [] (B.repeat 0 :x))
  (assert= [:x :x :x :x :x :x] (B.repeat 6 :x)))

(fn tests.flatten-ignore []
  (assert= [] (B.flatten 1) "ignores number args")
  (assert= [] (B.flatten :a "ignores string args"))
  (assert= [] (B.flatten {:a 1} "ignores hash args")))

(fn tests.flatten []
  (assert= [] (B.flatten) "always returns a table")
  (assert= [1] (B.flatten [[[[1]]]]) "flattens nested tables")
  (assert= [5 4 3 2 1] (B.flatten [[[[5] 4] 3 2] 1]) "flattens all items")
  (assert= [5 4 {:x [:y :z]} 3 2 1] (B.flatten [[[[5] 4] {:x [:y :z]} 3 2] 1])
           "does not flatten hashes"))

(fn shuffle-test-impl [shuffle-fn]
  (let [orig (B.ranget 100)]
    (var loop-count 0)
    (for [i 1 10 :until (not (B.deep= orig (shuffle-fn orig)))]
      (set loop-count i))
    (assert-not= 10 loop-count "should eventually return a shuffled array"))
  (assert= (B.ranget 100) (B.sort! (shuffle-fn (B.ranget 100)))
           "always contains the same elements"))

;; shuffle and shuffle! use slightly different algorithms
(fn tests.shuffle []
  (shuffle-test-impl B.shuffle))

(fn tests.shuffle! []
  (shuffle-test-impl (B.comp B.shuffle! B.copy)))

(fn tests.rand-nth []
  (let [items [:a :b :c :d 1 2 3 4]]
    (for [i 1 10]
      (let [selected-item (B.rand-nth items)]
        (is (B.any? (partial = selected-item) items)
            "rand-nth always returns items from the input table")))))

(fn tests.sort! []
  (assert= (B.ranget 10) (B.sort! (B.ranget 10 1 -1))
           "sort works at all")
  (assert= (B.ranget 10 1 -1) (B.sort! (B.ranget 10) #(< $2 $1))
           "sort can take a comparison function"))

(fn tests.sort-by! []
  (assert= (B.ranget 10) (B.sort-by! B.identity (B.ranget 10 1 -1))
           "identity works the same as sort!")
  (assert= (B.ranget 10 1 -1) (B.sort-by! #(- $) (B.ranget 10))
           "can reverse with a custom key fn")
  (let [unsorted (B.mapt #{:x $} (B.range 10 1 -1))
        expected (B.mapt #{:x $} (B.range 10))]
    (assert= expected (B.sort-by! #$.x unsorted)
             "can sort by a table lookup"))
  (is (not (pcall #(B.sort-by! #[$ $] (B.range 10))))
      "can't sort by a table"))


;;; kv-tables aka maps

(fn tests.assoc! []
  (assert= {:a 1} (B.assoc! {:a 1}))
  (assert= {:a 1 :b 2 :c 3 :d 4} (B.assoc! {:a 1} :b 2 :c 3 :d 4))
  (assert= [:a :b :c] (B.assoc! [:a] 2 :b 3 :c)
           "works on arrays")
  (assert= {:a 1} (B.assoc! {:a 1} :b nil)
           "assoc with a nil value is a no-op")
  (is (not (pcall #(B.assoc! {:a 1} nil 2)))
      "nil key is an error"))

(fn tests.assoc-in! []
  (assert= {:a 1} (B.assoc-in! {} :a 1) "one key is the same as assoc!")
  (assert= {:a {:b {:c 1}}} (B.assoc-in! {} :a :b :c 1) "multiple keys")
  (assert= {:a {:b {:c 1 :y 3} :x 2}} (B.assoc-in! {:a {:x 2 :b {:y 3}}} :a :b :c 1)
           "does not clobber existing tables")
  (assert= {:a {:b {:c 1}}} (B.assoc-in! {:a {:b {:c {:d 2}}}} :a :b :c 1)
           "clobbers the last value if it is a table"))

(fn tests.dissoc! []
  (assert= {:a 1 :b 2 :c 3} (B.dissoc! {:a 1 :b 2 :c 3}))
  (assert= {:a 1} (B.dissoc! {:a 1 :b 2 :c 3} :b :c))
  (assert= {} (B.dissoc! {:a 1} :a))
  (assert= [:a] (B.dissoc! [:a :b :c] 2 3)
           "works on arrays")
  (assert= {:a 1} (B.dissoc! {:a 1} :b :c)
           "missing key is a no-op")
  (is (not (pcall #(B.dissoc! {:a 1} nil)))
      "nil key is an error"))

(fn tests.update! []
  (assert= {:a 1} (B.update! {:a 1} :a B.identity))
  (assert= {:a 2} (B.update! {:a 1} :a B.inc))
  (assert= {:a 10} (B.update! {:a 1} :a B.clamp 10 20)
           "passes value as first arg to function")
  (assert= [10 10 8] (B.update! [10 9 8] 2 B.inc)
           "works on arrays")
  (assert= {:a 1} (B.update! {} :a #1)
           "called with nil with key doesn't exist")
  (is (not (pcall #(B.update! {} :a B.inc)))))

(fn tests.keys []
  (assert-items= [] (B.keys {}))
  (assert-items= [:a :b :c] (B.keys {:a 1 :b 2 :c 3}))
  (assert-items= [1 2 3] (B.keys [:a :b :c]))
  (is (not (pcall #(B.keys 1)))))

(fn tests.vals []
  (assert-items= [] (B.vals {}))
  (assert-items= [1 2 3] (B.vals {:a 1 :b 2 :c 3}))
  (assert-items= [:a :b :c] (B.vals [:a :b :c]))
  (is (not (pcall #(B.vals 1)))))

(fn tests.copy []
  (let [orig-a (B.ranget 10)]
    (assert= orig-a (B.copy orig-a) "same value")
    (is (not= orig-a (B.copy orig-a)) "not identical"))
  (let [orig-b [[1] 2 3 4]]
    (assert= orig-b (B.copy orig-b))
    (is (= (. orig-b 1) (. (B.copy orig-b) 1)) "shallow copy was made")))

(fn tests.deep-copy []
  (let [orig-a (B.ranget 10)]
    (assert= orig-a (B.deep-copy orig-a) "same value")
    (is (not= orig-a (B.deep-copy orig-a)) "not identical"))
  (let [orig-b [[[[1]]] {:a {:b {:c [:xyz]}}} 2 3 4]]
    (assert= orig-b (B.deep-copy orig-b))
    (is (not= (. orig-b 1 1 1) (. (B.deep-copy orig-b) 1 1 1))
        "array deep copy")
    (is (not= (. orig-b 2 :a :b :c) (. (B.deep-copy orig-b) 2 :a :b :c))
        "hash deep copy"))
  ;; array with a cycle
  (let [orig-c [1 2 [[3]]]]
    (tset orig-c 3 1 2 orig-c)
    (assert= orig-c (B.deep-copy orig-c) "array with cycle is equal")
    (is (not= orig-c (B.deep-copy orig-c)) "array with cycle is not identical"))
  ;; hash with a cycle
  (let [orig-d {:a 1 :b {:c {}}}]
    (tset orig-d :b :c orig-d orig-d)
    (assert= orig-d (B.deep-copy orig-d) "hash with cycle is equal")
    (is (not= orig-d (B.deep-copy orig-d)) "hash with cycle is not identical")
    (is (= orig-d (next (. (B.deep-copy orig-d) :b :c)))
        "key has not been copied")))

(fn tests.deep-copy-with-keys []
  (let [orig {:a 1 :b {:c {}}}
        ;; cycle wher the key itself creates a cycle
        _ (tset orig :b :c orig orig)
        ;; cycle where just the value is a cycle
        _ (tset orig :z orig)
        copy (B.deep-copy-with-keys orig)]
    (assert-not= orig copy
                 "when copying keys, luaunit doesn't detect equality")
    ;; remove the key cycle and contents should compare equal
    (tset orig :b :c {})
    (tset copy :b :c {})
    (assert= orig copy "other than the copied key, contents are equal")))

(fn tests.select-keys []
  (assert= {} (B.select-keys {:a 1}) "no keys")
  (assert= {} (B.select-keys {} :a :b) "empty source table")
  (assert= {:a 1} (B.select-keys {:a 1} :a :b))
  (assert= {:a 1} (B.select-keys {:a 1 :b 2} :a))
  (assert= [:a :b :c] (B.select-keys [:a :b :c :d :e :f :g] 1 2 3) "array")
  (let [orig {:a {:b {:c 1}}}
        copy (B.select-keys orig :a)]
    (assert= orig copy)
    (is (not= orig copy) "returns a copy")
    (is (= (. orig :a) (. copy :a)) "shallow copy")))

(fn tests.deep-select-keys []
  (assert= {} (B.deep-select-keys {:a 1}) "no keys")
  (assert= {} (B.deep-select-keys {} :a :b) "empty source table")
  (assert= {:a 1} (B.deep-select-keys {:a 1} :a :b))
  (assert= {:a 1} (B.deep-select-keys {:a 1 :b 2} :a))
  (assert= [:a :b :c] (B.deep-select-keys [:a :b :c :d :e :f :g] 1 2 3) "array")
  (let [orig {:a {:b {:c 1}}}
        copy (B.deep-select-keys orig :a)]
    (assert= orig copy)
    (is (not= orig copy) "returns a copy")
    (is (not= (. orig :a :b) (. copy :a :b)) "deep copy")))

(fn tests.merge! []
  (assert= nil (B.merge! nil) "returns nil unchanged")
  (assert= {} (B.merge! {}) "returns table unchanged")
  (assert= {:a 1 :b 2 :c 3} (B.merge! {:a 1} {:b 2} {:c 3})
           "merges any number of tables")
  (assert= {:a 10 :b 20 :c 3} (B.merge! {:a 1} {:b 2} {:c 3} {:a 10 :b 20})
           "later values take precedence")
  (let [x {}]
    (is (= x (B.merge! x {:a 1} {:b 2}))
        "merges into the original table"))
  (assert= [:a 2 3] (B.merge! [1 2 3] [:a])
           "array overwrites original table"))

(fn tests.merge []
  (assert= {:a 1 :b 2} (B.merge {} {:a 1} {:b 2}))
  (let [x {}]
    (is (not= x (B.merge x {:a 1} {:b 2}))
        "creates a copy")))

(fn tests.merge-with! []
  (assert= {:a 1 :b 9} (B.merge-with! #(+ $1 $2) {:a 1 :b 2} {:b 3} {:b 4}))
  (assert= {:a [1 2 3] :b [4] :c [5]}
           (B.merge-with! B.into! {:a [1] :b [4]} {:a [2 3]} {:c [5]}))
  (assert= {:a 1 :b "234" :c 5}
           (B.merge-with! #(.. $1 $2) {:a 1 :b 2} {:b 3} {:b 4} {:c 5})
           "merge function only called with key collision"))

(fn tests.deep-merge! []
  (assert= {:a {:b {:c [1 2 3] :d [:x :y :z] :e {:f "hello"}}}}
           (B.deep-merge! {:a {:b {:c [1 2 3]}}}
                          {:a {:b {:d [:x :y :z] :e {:f "hello"}}}})
           "merges hashes")
  (assert= {:a {:b {:c [:x]}}}
           (B.deep-merge! {:a {:b {:c [1 2 3]}}}
                          {:a {:b {:c [:x]}}})
           "overwrites arrays"))

(fn tests.deep-merge []
  (let [x {:a {:b {:c [1 2 3]}}}]
    (assert= {:a {:b {:c [1 2 3] :d [:x :y :z] :e {:f "hello"}}}}
             (B.deep-merge x {:a {:b {:d [:x :y :z] :e {:f "hello"}}}})
             "merges hashes")
    (assert= {:a {:b {:c [1 2 3]}}} x
             "deep copy made")))

(fn tests.deep-merge-with! []
  (assert= {:a {:b {:c [1 2 3 :x]}}}
           (B.deep-merge-with! B.into!
                               {:a {:b {:c [1 2 3]}}}
                               {:a {:b {:c [:x]}}})
           "calls merge function with arrays")
  (assert= {:a {:b {:c [:x 2 3]}}}
           (B.deep-merge-with! B.merge!
                               {:a {:b {:c [1 2 3]}}}
                               {:a {:b {:c [:x]}}})
           "can still use regular merge"))

(fn tests.deep-merge-with []
  (let [x {:a {:b {:c [1 2 3]}}}]
    ;; Note this only works if the merge fn also makes a copy, so e.g. into!
    ;; won't work!
    (assert= {:a {:b {:c [:x 2 3]}}}
             (B.deep-merge-with B.merge x {:a {:b {:c [:x]}}})
             "calls merge function with arrays")
    (assert= {:a {:b {:c [1 2 3]}}} x
             "deep copy made")))

tests
