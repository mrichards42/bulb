(import-macros {: assert= : is} :test.macros)
(local B (require :bulb))
(local unpack B.unpack)

(local tests {})

(fn std-comp-tests [comp-fn]
  (assert= :x ((comp-fn) :x) "0 arg comp is identity")
  (is (= B.inc (comp-fn B.inc)) "1 arg comp returns the original function")
  (let [;; http://oeis.org/A075427
        fns (B.flatten (B.repeat 10 [#(* 2 $) #(+ $ 1)]))
        results [1 2 3 6 7 14 15 30 31 62 63 126 127 254 255 510 511 1022 1023 2046]]
    (for [n 1 20]
      (assert= (. results n) ((comp-fn (unpack fns (- 21 n))) 0)
               (.. "composes " n " functions, right to left")))))

(fn tests.comp []
  (std-comp-tests B.comp)
  (assert= 220 ((B.comp #(B.sum [$...])
                        #(unpack (B.totable (B.mapcat B.range [$...]))))
                (values 1 2 3 4 5 6 7 8 9 10))
           "passes multiple args between functions"))

(fn tests.comp1 []
  (std-comp-tests B.comp1)
  (assert= 1 ((B.comp1 #(B.sum [$...])
                       #(unpack (B.totable (B.mapcat B.range [$...]))))
              (values 1 2 3 4 5 6 7 8 9 10))
           "passes only single arg between functions"))

(fn tests.comp2 []
  (std-comp-tests B.comp2)
  (assert= 2 ((B.comp2 #(B.sum [$...])
                       #(unpack (B.totable (B.mapcat B.range [$...]))))
              (values 1 2 3 4 5 6 7 8 9 10))
           "passes only two args between functions"))

(fn tests.juxt []
  (assert= [1] [((B.juxt B.first) (B.ranget 10))])
  (assert= [1 2] [((B.juxt B.first B.second) (B.ranget 10))])
  (assert= [1 2 10] [((B.juxt B.first B.second B.last) (B.ranget 10))])
  (assert= [1 2 10 [1 2 3 4 5 6 7 8 9]]
           [((B.juxt B.first B.second B.last B.butlast) (B.ranget 10))]))

(fn tests.complement []
  (assert= false ((B.complement B.empty?) []))
  (assert= true ((B.complement B.pos?) -1))
  (assert= false ((B.complement B.pos?) 1)))

(fn tests.fnil []
  (assert= {:a 1 :b 10} (-> {:b 9}
                            (B.update! :a (B.fnil B.inc 0))
                            (B.update! :b (B.fnil B.inc 0))))
  (assert= [:x] ((B.fnil B.conj! [] :x)) "can nil-patch 2 args")
  (assert= [:x :y] ((B.fnil B.conj! [] :x :y)) "can nil-patch 3 args")
  (assert= [:a :b :c :x :y :z 1 2 3]
           ((B.fnil B.conj! [] :x :y :z 1 2 3) [:a :b :c])
           "can nil-patch any number of args")
  (assert= [:a :b :c 1 2 3 :last]
           ((B.fnil B.conj! [] :x :y :z) [:a :b :c] 1 2 3 :last)
           "does not overwrite explicit args"))

tests
