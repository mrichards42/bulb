(local B
  {:__NAME "Bulb"
   :__DOC "Fennel 'core' library."})

(macro declare [...]
  `(var (,...) nil))

(macro defn [name ...]
  `(values ,(if (in-scope? name)
              `(set ,name (fn ,name ,...)) ; forward declaration
              `(fn ,name ,...))
           (tset ,(sym :B) ,(tostring name) ,name)))

;; [x] TODO assoc, dissoc, update
;; [x] TODO take-last drop-last?
;; [x] TODO even?, odd?, pos?, neg?, other math functions, other predicates, like table? true?
;; false? nil?
;; [x] TODO flatten?
;; [ ] TODO memoize
;; [x] TODO shuffle
;; [x] TODO slurp?, spit?
;; [ ] TODO every-pred?, some-fn? <-- not great names
;; [x] TODO: take-nth
;; [x] TODO interleave

;; TODO: should we have partition into varargs as well?

(local mfloor math.floor)
(local mrandom math.random)
(local tinsert table.insert)
(local tsort table.sort)
(local sformat string.format)
(local unpack (or table.unpack _G.unpack))

;; lua has a 200 locals limit, and there are a lot of locals in this file, so
;; it's split up into 2 giant `do` forms -- this one for the top half of the
;; file, and a second one for the iterators section
(do

;;;; == Predicates and coercions ==============================================

;; primitives

(defn table? [x] (= :table (type x)))
(defn string? [x] (= :string (type x)))
(defn number? [x] (= :number (type x)))
(defn function? [x] (= :function (type x)))
(defn userdata? [x] (= :userdata (type x)))
(defn nil? [x] (= :nil (type x)))
(defn boolean? [x] (= :boolean (type x)))
(defn true? [x] (= true x))
(defn false? [x] (= false x))

;; functions

(defn callable? [x]
  "Is `x` callable? Returns true for functions and anything with a `__call`
  metamethod."
  (or (= :function (type x))
      (and (-?> (getmetatable x) (. :__call)) true)
      false))

;; bools

(defn boolean [x]
  (if x true false))

;; tables

(defn empty? [x]
  "Returns true if `x` is an empty table or string."
  (match (type x)
    :table (= nil (next x))
    :string (= "" x)))

(defn not-empty [x]
  "Returns `x`, or nil if `x` is an empty table or string."
  (if (empty? x) nil x))

(defn array? [x]
  "Is `x` an array table? Returns true for tables with a non-nil first item (or
  are entirely empty) that are not callable."
  (and (= :table (type x))
       (or (not= nil (. x 1)) (= nil (next x)))
       (not (callable? x))))

(defn hash? [x]
  "Is `x` a hash table? Returns true for non-empty tables with a nil first item
  that are not callable."
  (and (= :table (type x))
       (= nil (. x 1))
       (not= nil (next x))
       (not (callable? x))))

(defn hash-or-empty? [x]
  "Is `x` a hash table or an empty table? Returns true for tables with a nil
  first item that are not callable."
  (and (= :table (type x))
       (= nil (. x 1))
       (not (callable? x))))

(fn deep=-helper [a b seen]
  (if
    (= a b) true
    (not= :table (type a)) false
    (not= :table (type b)) false
    (?. seen a b) true
    (do
      (if (. seen a)
        (tset seen a b true)
        (tset seen a {b true}))
      ;; check b's keys first (just that they exist in `a` at all)
      (each [k _ (pairs b)]
        (when (= nil (. a k))
          (lua "do return false end")))
      ;; then check a's keys and both values simultaneously
      (each [k v (pairs a)]
        (when (not (deep=-helper (. a k) (. b k) seen))
          (lua "do return false end")))
      true)))

(defn deep= [a b]
  "Returns true when `a` and `b` are equal. Compares hash-table keys by
  identity, and everything else by value."
  (if
    (= a b) true
    (not= :table (type a)) false
    (not= :table (type b)) false
    (deep=-helper a b {})))

;; numbers

(defn int? [x]
  "Returns true if `x` is a number without a fractional part."
  (and (= :number (type x))
       (= x (mfloor x))))

(defn float? [x]
  "Returns true if `x` is a number with a fractional part."
  (and (= :number (type x))
       (not= x (mfloor x))))

(defn zero? [x] (= 0 x))
(defn pos? [x] (< 0 x))
(defn neg? [x] (< x 0))
(defn even? [x] (= 0 (% x 2)))
(defn odd? [x] (= 1 (% x 2)))



;;;; == Math ==================================================================

(defn inc [x] (+ x 1))
(defn dec [x] (- x 1))
(defn clamp [x min max]
  (if
    (< x min) min
    (< max x) max
    x))



;;;; == Tables ================================================================

;; array tables

(defn conj! [tbl ...]
  "Appends all values to `tbl`, returning `tbl`."
  (var end (length tbl)) ; a bit faster than table.insert
  (for [i 1 (select "#" ...)]
    (match (select i ...)
      x (do (set end (+ 1 end))
          (tset tbl end x))))
  tbl)

(fn flatten-into! [tbl last-idx xs]
  (var last last-idx)
  (for [i 1 (length xs)]
    (let [x (. xs i)]
      (if (array? x)
        (set last (flatten-into! tbl last x))
        (do (tset tbl last x)
          (set last (+ last 1))))))
  last)

(defn flatten [tbl]
  "Flattens nested array tables into a single array (does not flatten hashes)."
  (let [ret []]
    (when (array? tbl)
      (flatten-into! ret 1 tbl))
    ret))
 
(defn shuffle! [tbl]
  "Shuffles `tbl` in place."
  ;; Fisher-yates
  (for [i (length tbl) 2 -1]
    (let [j (mrandom i)
          tmp (. tbl i)]
      (tset tbl i (. tbl j))
      (tset tbl j tmp)))
  tbl)

(defn shuffle [tbl]
  "Shuffles `tbl`, returning a new table."
  ;; The "inside-out" algorithm
  (let [ret [(. tbl 1)]]
    (for [i 2 (length tbl)]
      (let [j (mrandom i)]
        (when (not= j i)
          (tset ret i (. ret j)))
        (tset ret j (. tbl i))))
    ret))

(defn rand-nth [tbl]
  "Returns a random item in `tbl`."
  (. tbl (mrandom (length tbl))))

(defn sort! [tbl ?cmp]
  "Sorts `tbl` in place, optionally with a comparison function."
  (tsort tbl ?cmp)
  tbl)

(defn sort-by! [f tbl]
  "Sorts `tbl` in place by the result of calling `f` on each item."
  (tsort tbl (fn [a b] (< (f a) (f b))))
  tbl)

;; key/value tables (aka hashes, maps)

(defn assoc! [tbl ...]
  "Adds any number of key/value pairs to `tbl`, returning `tbl`. Like [[tset]]
  but for multiple pairs."
  (for [i 1 (select "#" ...) 2]
    (let [(k v) (select i ...)]
      (tset tbl k v)))
  tbl)

(defn assoc-in! [tbl ...]
  "(assoc-in! tbl key keys... val)

  Sets a value in a nested table. Like [[tset]], but creates missing
  intermediate tables."
  (let [n (select "#" ...)
        last-k (select (- n 1) ...)
        val (select n ...)]
    (var t tbl)
    (for [i 1 (- n 2)]
      (let [k (select i ...)]
        (when (= nil (. t k))
          (tset t k {}))
        (set t (. t k))))
    (tset t last-k val)
    tbl))

(defn dissoc! [tbl ...]
  "Removes any number of keys from `tbl`, returning `tbl`."
  (for [i 1 (select "#" ...)]
    (let [k (select i ...)]
      (tset tbl k nil)))
  tbl)

(defn update! [tbl k f ...]
  "Updates the value of `k` in `tbl` by calling `f` on its current value. Any
  additional args are passed after the table value, i.e. (f (. tbl k) ...)"
  (tset tbl k (f (. tbl k) ...))
  tbl)

(defn keys [tbl]
  "Returns all keys in `tbl`."
  (icollect [k _ (pairs tbl)]
    k))

(defn vals [tbl]
  "Returns all values in `tbl`."
  (icollect [_ v (pairs tbl)]
    v))

(defn copy [tbl]
  "Returns a (shallow) copy of `tbl`."
  (collect [k v (pairs tbl)]
    (values k v)))

(fn deep-copy-helper [x seen copy-keys?]
  (if
    (not= :table (type x)) x
    (. seen x) (. seen x)
    (let [ret {}]
      (tset seen x ret)
      (each [k v (pairs x)]
        (let [k* (if copy-keys?
                   (deep-copy-helper k seen copy-keys?)
                   k)]
          (tset ret k* (deep-copy-helper v seen copy-keys?))))
      ret)))

(defn deep-copy [x]
  "Returns a deep copy of `tbl`. Does not copy hash-table keys that are tables,
  since generally table keys are compared by identity, not value."
  (deep-copy-helper x {} false))

(defn deep-copy-with-keys [x]
  "Like [[deep-copy]], but _does_ copy hash-table keys."
  (deep-copy-helper x {} true))

(defn select-keys [tbl ...]
  "Returns a (shallow) copy of `tbl` that only includes the given `keys` (the
  remaining args)."
  (let [ret {}]
    (for [i 1 (select "#" ...)]
      (let [k (select i ...)]
        (tset ret k (. tbl k))))
    ret))

(defn deep-select-keys [tbl ...]
  "Returns a deep copy of `tbl` that only includes the given `keys` (the
  remaining args)."
  (let [ret {}]
    (for [i 1 (select "#" ...)]
      (let [k (select i ...)]
        (let [v (. tbl k)]
          (if (= :table (type v))
            (tset ret k (deep-copy v))
            (tset ret k v)))))
    ret))

(defn merge! [tbl ...]
  "Merges any number of other tables into `tbl`, in place. Ignores nils."
  (let [others [...]]
    (for [i 1 (select "#" ...)]
      (match (. others i)
        other (each [k v (pairs other)]
                (tset tbl k v))))
    tbl))

(defn merge [...]
  "Like [[merge!]] but returns a new table."
  (merge! {} ...))

(defn merge-with! [f tbl ...]
  "Merges any number of other tables into `tbl`, in place. When the same key
  exists in two tables, calls (f left right) and uses the result. Ignores
  nils."
  (let [others [...]]
    (for [i 1 (select "#" ...)]
      (match (. others i)
        other (each [k v (pairs other)]
                (tset tbl k
                      (match (. tbl k)
                        oldv (f oldv v)
                        _ v))))))
  tbl)

(defn merge-with [f ...]
  "Like [[merge-with!]] but returns a new table."
  (merge-with! f {} ...))

(fn deep-merge-helper [merge-fn f a b]
  (if (and (hash-or-empty? a) (hash-or-empty? b))
    (merge-fn f a b)
    (f a b)))

(defn deep-merge-with! [f ...]
  "Merges any number of nested hash tables, recursively, in place. When the
  same key exists in two tables, and the values are not both hash tables, calls
  (f left right) and uses the result. Ignores nils."
  (merge-with! (fn [a b] (deep-merge-helper deep-merge-with! f a b)) ...))

(defn deep-merge-with [f ...]
  "Like [[deep-merge-with]], but returns a new table."
  (merge-with (fn [a b] (deep-merge-helper deep-merge-with f a b)) ...))

(fn second-arg [_ b] b)

(defn deep-merge! [...]
  "Merges any number of nested hash tables, recursively, in place. Overwrites
  array tables instead of merging them. Ignores nils."
  (deep-merge-with! second-arg ...))

(defn deep-merge [...]
  "Like [[deep-merge!]] but returns a new table."
  (deep-merge-with second-arg ...))




;;;; == Functional ============================================================

(defn comp [...]
  "Takes any number of functions and composes them together in order, passing
  all values from one function to the next:
  ((comp f g) x y z) -> (f (g x y z))"
  (match (select "#" ...)
    1 ...
    2 (let [(f g) ...] (fn [...] (f (g ...))))
    3 (let [(f g h) ...] (fn [...] (f (g (h ...)))))
    4 (let [(f g h x) ...] (fn [...] (f (g (h (x ...))))))
    5 (let [(f g h x y) ...] (fn [...] (f (g (h (x (y ...)))))))
    6 (let [(f g h x y z) ...] (fn [...] (f (g (h (x (y (z ...))))))))
    n (comp (comp (pick-values 6 ...)) (select 7 ...))))

(macro comp-body [pick-values-n ...]
  (fn body1 [args f ...]
    (let [syms (list)]
      (for [i 1 pick-values-n]
        (tset syms i (gensym (tostring f))))
      (if ...
        `(let [,syms ,(list f (unpack args))]
           ,(body1 syms ...))
        `(let [,syms ,(list f (unpack args))]
           (values ,(unpack syms))))))
  `(fn [...] ,(body1 [`...] ...)))

(defn comp1 [...]
  "Takes any number of functions and composes them together in order, passing
  only 1 value from one function to the next:
  ((comp1 f g) x) -> (pick-values 1 (f (pick-values 1 (g x))))"
  (match (select "#" ...)
    1 ...
    2 (let [(f g) ...] (comp-body 1 f g))
    3 (let [(f g h) ...] (comp-body 1 f g h))
    4 (let [(f g h x) ...] (comp-body 1 f g h x))
    5 (let [(f g h x y) ...] (comp-body 1 f g h x y))
    6 (let [(f g h x y z) ...] (comp-body 1 f g h x y z))
    n (let [(f g h x y z) ...]
        (comp1 (comp1 f g h x y z) (select 7 ...)))))

(defn comp2 [...]
  "Takes any number of functions and composes them together in order, passing
  only 2 values from one function to the next:
  ((comp2 f g) x y) -> (pick-values 2 (f (pick-values 2 (g x y))))"
  (match (select "#" ...)
    1 ...
    2 (let [(f g) ...] (comp-body 2 f g))
    3 (let [(f g h) ...] (comp-body 2 f g h))
    4 (let [(f g h x) ...] (comp-body 2 f g h x))
    5 (let [(f g h x y) ...] (comp-body 2 f g h x y))
    6 (let [(f g h x y z) ...] (comp-body 2 f g h x y z))
    n (let [(f g h x y z) ...]
        (comp2 (comp2 f g h x y z) (select 7 ...)))))

(defn juxt [...]
  "Takes any number of functions and returns a fn that returns multiple values,
  calling each function in turn:
  ((juxt f g h) x) -> (values (f x) (g x) (h x))"
  (match (select "#" ...)
    1 ...
    2 (let [(f g) ...] (fn [...] (values (f ...) (g ...))))
    3 (let [(f g h) ...] (fn [...] (values (f ...) (g ...) (h ...))))
    n (let [fs [...]] (fn [...] (unpack (icollect [_ f (ipairs fs)] (f ...)))))))

(defn identity [...]
  "Returns all arguments."
  ...)

(defn identity1 [x]
  "Returns just the first argument."
  x)

(defn complement [f]
  "Returns a function that calls `f` and returns its opposite."
  (fn [...] (not (f ...))))

(defn fnil [f x ...]
  "Returns a function that calls `f`, replacing a nil first argument with `x`.
  Takes any number of args, patching as many nil arguments to `f`."
  (match (select "#" ...)
    0 (fn [a ...]
        (let [a (if (= nil a) a x)]
          (f a ...)))
    1 (let [y ...]
        (fn [a b ...]
          (let [a (if (= nil a) a x)
                b (if (= nil b) b y)]
            (f a b ...))))
    2 (let [(y z) ...]
        (fn [a b c ...]
          (let [a (if (= nil a) a x)
                b (if (= nil b) b y)
                c (if (= nil c) c z)]
            (f a b c ...))))
    _ (let [replacements [x ...]
            n (length replacements)]
        (fn [...]
          (let [args [...]
                count (select "#" ...)]
            (for [i 1 n]
              (when (= nil (. args i))
                (tset args i (. replacements i))))
            (f (unpack args 1 count)))))))

(local delay-none {})
(defn delay-fn [f]
  "Returns a 'delayed' function. When called the first time, calls `f` and
  caches the result for subsequent calls."
  (var result delay-none)
  (fn []
    (when (= delay-none result)
      (set result [(f)]))
    (unpack result)))



;;;; == IO ====================================================================

(defn slurp [filename ?mode]
  "Reads the full contents of `filename`, optionally with `mode` (default :r)."
  (with-open [f (assert (io.open filename (or ?mode :r)))]
    (f:read :*)))

(defn spit [filename contents ?mode]
  "Writes `contents` to `filename`, optionally with `mode` (default :w)."
  (with-open [f (assert (io.open filename (or ?mode :w)))]
    (f:write contents)))

(defn pprint [...]
  "Like [[print]], but calls [[fennel.view]] on each argument first."
  (let [{: view} (require :fennel)
        formatted []
        n (select "#" ...)]
    (for [i 1 n]
      (let [x (select i ...)]
        (tset formatted i (view x))))
    (print (unpack formatted 1 n))))

(defn printf [fmt ...]
  "Calls [[string.format]] on all arguments, and prints the result without a
  trailing newline."
  (io.write (sformat fmt ...)))



;;;; == Module utils ==========================================================

(defn unrequire [mod-name]
  "Marks `mod-name` as 'not yet required', so that the next `require` will
  reload the module. Works on both lua modules and fennel macro modules."
  (tset package :loaded mod-name nil)
  (pcall #(tset (require :fennel) :macro-loaded mod-name nil)))

(defn require! [mod-name]
  "Reloads a module."
  (unrequire mod-name)
  (require mod-name))


) ; end do


;;;; == Iterators =============================================================
(do ; see note about locals at the top of the file

;;; Stateful vs stateless

;; All iterators in this module are stateful, i.e. they are 0-arg functions,
;; and any state is kept in a closure.  This makes the implementation of most
;; functions simpler, since an there is only one value to keep track of (the
;; function) per iterator instead of three (function, state, control).
;; Functions like [[zip]] that use multiple iterators are made significantly
;; simpler, though of course there are trade-offs.

;; The main trade-off is that these iterators are mutable, so you can't copy
;; them around. For instance, [[cycle]] would be much simpler with stateless
;; iterators.

;; On the other hand, while some of Lua's builtin iterators are stateless
;; (ipairs and pairs), other are not (string.gmatch and io.lines). Rather than
;; leaving statefulness or statelessness up to the source iterator, it seemed
;; simplest to make all iterators stateful and be done with it.

;; For a similar library which attempts to preserve stateless iterators, see
;; luafun.  The main disadvantage is that the first value of each of luafun's
;; iterators is a `state` value which is intended to be discarded from the end
;; user, but shows up if you iterate manually with for/each, so they provide an
;; `each` function to deal with that.


;;; Multi-value/varargs treatment

;; Most functions in this module know how to handle multiple-value iterators.
;; Functions that only use the first value returned by an iterator are marked
;; with "only supports single-value iterators". Some functions come in
;; single-value and multi-value version: the multi-value version is marked with
;; `+` (e.g. [[totable]] and [[totable+]]).

;; Since multi-vals are not first-class in Lua, we have to jump through some
;; hoops to get this support. In most situations this means an extra function,
;; and ends up looking sort of like continuation-passing-style. See [[map]]
;; (one helper function), and especially [[keep]] (two helper functions) for
;; examples of this style, although almost every function in this module that
;; handles multi-vals uses helper functions. When possible, these are lifted
;; out as (top-level module) locals for efficiency, rather than creating a new
;; closure each time.

(local {: array? : callable? : clamp : complement} B) ;; locals from above
(declare iter-cached cached-iter?)

;;; -- Basic iterators and predicates -----------------------------------------

(defn iterable? [x]
  "Is `x` iterable? Returns true for tables, strings, functions, and anything
  with a `__call` metamethod."
  (match (type x)
    :table true
    :function true
    :string true
    _ (callable? x)))

(fn nil-iter []) ; always returns nil; for internal use only

(local ipairs-iter (ipairs []))

(defn iter-indexed [tbl]
  "Iterates over index/value pairs in `tbl`, starting from 1. A stateful
  version of [[ipairs]]; identical to (iter (ipairs tbl))."
  (var i 0)
  (var end (length tbl))
  (fn []
    (when (< i end)
      (set i (+ i 1))
      (values i (. tbl i)))))

(defn iter-kv [tbl]
  "Iterates over key/value pairs in `tbl`. A stateful version of [[pairs]];
  identical to (iter (pairs tbl))."
  (var last-key nil)
  (fn []
    (let [(k v) (next tbl last-key)]
      (when (not= nil k)
        (set last-key k)
        (values k v)))))

(defn wrap-iter [it state ctrl_]
  "Wraps a stateless lua iterator, returning a stateful (single function)
  iterator. Typically [[iter]] should be used instead, as it will call this
  function when necessary to wrap a stateless iterator."
  (var ctrl ctrl_)
  (fn step [...]
    (when ...
      (set ctrl ...)
      ...))
  (fn []
    (step (it state ctrl))))

(defn iter [x ...]
  "Converts a table, function, or string into a stateful iterator. Called by
  all iterator functions to coerce their iterable arguments. Typically you only
  need to call this function to wrap a stateless iterator.

  Tables are assumed to be arrays and iterate over values, starting from one.
  Use [[iter-kv]] to iterate over key/value pairs of hash tables, or the
  equivalent (iter (pairs tbl)). Use [[iter-indexed]] to iterate over
  index/value pairs of arrays, or the equivalent (iter (ipairs tbl)).

  Strings iterate over each character.

  Functions (and callable tables) are assumed to already be stateful iterators
  when passed no additional arguments. With additional arguments, functions are
  assumed stateless iterators, and are wrapped using [[wrap-iter]]."
  (match (type x)
    :function (if
                (= 0 (select "#" ...)) x
                ;; stateless pairs
                (= next x) (iter-kv ...)
                ;; stateless ipairs
                (= ipairs-iter x) (iter-indexed ...)
                ;; otherwise we have to wrap this
                (wrap-iter x ...))
    :table (if
             (cached-iter? x) (x:copy)
             (callable? x) (if (= 0 (select "#" ...))
                             x
                             (wrap-iter x ...))
             :else (do
                     (var i 0)
                     (fn []
                       (set i (+ i 1))
                       (. x i))))
    :string (do
              (var i 0)
              (var end (length x))
              (fn []
                (when (< i end)
                  (set i (+ i 1))
                  (x:sub i i))))))

(defn iterate [f ...]
  "Iterates over `f` and any initial values, producing a sequence of

  inits, (f inits), (f (f inits)), ...

  `f` must return the same number of values as it takes, e.g. (iterate f x y z)
  returns a 3-value iterator."
  (match (select "#" ...)
    0 f
    1 (let [init ...]
        (var x nil)
        (fn []
          (set x (if (not= nil x) (f x) init))
          x))
    2 (let [(init-x init-y) ...]
        (var (x y) (values nil nil))
        (fn []
          (set (x y) (if (not= nil x) (f x y) (values init-x init-y)))
          (values x y)))
    3 (let [(init-x init-y init-z) ...]
        (var (x y z) (values nil nil nil))
        (fn []
          (set (x y z) (if (not= nil x) (f x y z) (values init-x init-y init-z)))
          (values x y z)))
    _ (let [inits [...]]
        (var xs nil)
        (fn []
          (set xs (if (not= nil xs) [(f (unpack xs))] inits))
          (unpack xs)))))

(defn range [...]
  "(range)               -- infinite range
  (range end)            -- range from 1 to end
  (range start end)      -- range from start to end
  (range start end step) -- range from start to end increasing by step.

  Note that, following lua semantics, ranges start with 1 by default, and `end`
  is inclusive. In other words, these are equivalent:
    (each [x (range 1 10 3)] (print x))
    (for [x 1 10 3] (print x))"
  (match (select "#" ...)
    ;; infinite range from 1
    0 (do
        (var i 0)
        (fn [] (set i (+ 1 i)) i))
    1 (range 1 ...)
    _ (let [(start end step) ...]
        (if
          (not step) (range start end 1)
          (>= step 0) (do
                        (var i (- start step))
                        (fn []
                          (set i (+ i step))
                          (when (<= i end)
                            i)))
          :else (do
                  (var i (- start step))
                  (fn []
                    (set i (+ i step))
                    (when (>= i end)
                      i)))))))


;;; -- Table traversal --------------------------------------------------------

;; Starting with traversal "primitives" that only deal with tables. When passed
;; iterators, these functions just call the iterator version, i.e.
;;
;;     (totable (map ...))
;;
;; These are defined before the iterator versions b/c some iterator functions
;; (e.g. zip) use mapt.

(declare totable map keep filter remove zip)

(defn mapt [f ...]
  "Like [[map]], but collects results in a table."
  (if (and (= :table (type ...)) (= 1 (select "#" ...)))
    (let [tbl ...
          ret []]
      (for [i 1 (length tbl)]
        (tset ret i (f (. tbl i))))
      ret)
    ;; multiple args or not a table: use the iterator version
    (totable (map f ...))))

(defn keept [f ...]
  "Like [[keep]], but collects results in a table."
  (if (and (= :table (type ...)) (= 1 (select "#" ...)))
    (let [tbl ...
          ret []]
      (var last 1)
      (for [i 1 (length tbl)]
        (let [val (f (. tbl i))]
          (when val
            (tset ret last val)
            (set last (+ last 1)))))
      ret)
    ;; multiple args or not a table: use the iterator version
    (totable (keep f ...))))

(defn filtert [f iterable]
  "Like [[filter]], but collects results in a table."
  (if (= :table (type iterable))
    (let [tbl iterable
          ret []]
      (var last 1)
      (for [i 1 (length tbl)]
        (when (f (. tbl i))
          (tset ret last (. tbl i))
          (set last (+ last 1))))
      ret)
    ;; not a table: use the iterator version
    (totable (filter f iterable))))

(defn removet [f iterable]
  "Like [[remove]], but collects results in a table."
  (filtert (complement f) iterable))

(defn ranget [...]
  "Like [[range]], but collects results in a table."
  (totable (range ...)))

;; Hash table versions

(defn map-kv [f tbl]
  "Maps `f` over key/value pairs in `tbl`, returning a new table."
  (let [ret {}]
    (each [k v (pairs tbl)]
      (let [(k* v*) (f k v)]
        (if k* (tset ret k* v*))))
    ret))

(defn map-vals [f tbl]
  "Maps `f` over values in `tbl`, returning a new table."
  (map-kv (fn [k v] (values k (f v))) tbl))

(defn map-keys [f tbl]
  "Maps `f` over keys in `tbl`, returning a new table."
  (map-kv (fn [k v] (values (f k) v)) tbl))

(defn filter-kv [pred tbl]
  "Filters pairs of `tbl` where (pred key val) returns truthy."
  (let [ret []]
    (each [k v (pairs tbl)]
      (if (pred k v)
        (tset ret k v)))
    ret))

(defn filter-keys [pred tbl]
  "Filters pairs of `tbl` where (pred key) returns truthy."
  (filter-kv (fn [k _] (pred k)) tbl))

(defn filter-vals [pred tbl]
  "Filters pairs of `tbl` where (pred val) returns truthy."
  (filter-kv (fn [_ v] (pred v)) tbl))

(defn remove-kv [pred tbl]
  "Filters pairs of `tbl` where (pred key val) returns falsey."
  (filter-kv (complement pred) tbl))

(defn remove-keys [pred tbl]
  "Filters pairs of `tbl` where (pred key) returns falsey."
  (filter-kv (fn [k _] (not (pred k))) tbl))

(defn remove-vals [pred tbl]
  "Filters pairs of `tbl` where (pred val) returns falsey."
  (filter-kv (fn [_ v] (not (pred v))) tbl))

;;; -- Destructive traversal --------------------------------------------------

;; Same as the above table traversal functions, but mutating the table in-place
;; instead of returning a new one.

(defn map! [f tbl]
  "Maps `f` over `tbl` in place."
  (for [i 1 (length tbl)]
    (tset tbl i (f (. tbl i))))
  tbl)

(defn keep! [f tbl]
  "Maps `f` over `tbl` in place, keeping only truthy values."
  (let [end (length tbl)]
    (var last 1)
    (for [i 1 end]
      (let [val (f (. tbl i))]
        (when val
          (tset tbl last val)
          (set last (+ last 1)))))
    (for [i last end]
      (tset tbl i nil))
    tbl))

(defn filter! [pred tbl]
  "Removes values from `tbl` (in place) where `pred` returns falsey."
  (let [end (length tbl)]
    (var last 1)
    (for [i 1 end]
      (when (pred (. tbl i))
        (tset tbl last (. tbl i))
        (set last (+ last 1))))
    (for [i last end]
      (tset tbl i nil))
    tbl))

(defn remove! [pred tbl]
  "Removes values from `tbl` (in place) where `pred` returns truthy."
  (filter! (complement pred) tbl))

;; Destructive hash table versions

(defn map-vals! [f tbl]
  "Maps `f` over values in `tbl`, in place."
  (each [k v (pairs tbl)]
    (tset tbl k (f v)))
  tbl)

(defn remove-kv! [pred tbl]
  "Filters pairs of `tbl`, in place, where (pred key val) returns falsey."
  (each [k v (pairs tbl)]
    (when (pred k v)
      (tset tbl k nil)))
  tbl)

(defn remove-keys! [pred tbl]
  "Filters pairs of `tbl`, in place, where (pred key) returns falsey."
  (remove-kv! (fn [k _] (pred k)) tbl))

(defn remove-vals! [pred tbl]
  "Filters pairs of `tbl`, in place, where (pred val) returns falsey."
  (remove-kv! (fn [_ v] (pred v)) tbl))

(defn filter-kv! [pred tbl]
  "Filters pairs of `tbl`, in place, where (pred key val) returns truthy."
  (remove-kv! (fn [k v] (not (pred k v))) tbl))

(defn filter-keys! [pred tbl]
  "Filters pairs of `tbl`, in place, where (pred key) returns truthy."
  (remove-kv! (fn [k _] (not (pred k))) tbl))

(defn filter-vals! [pred tbl]
  "Filters pairs of `tbl`, in place, where (pred val) returns truthy."
  (remove-kv! (fn [_ v] (not (pred v))) tbl))


;; -- Extracting values -------------------------------------------------------

;; These mostly return values, not iterators (except for functions that return
;; slices, like rest/nthrest/butlast)

(declare drop-last)

(defn first [x]
  "Returns the first item in `x` (any iterable), or nil if empty.

  Note: for iterators, consumes the first value in x and returns it."
  (match (type x)
    :table (if (callable? x) (x) (. x 1))
    :string (x:sub 1 1)
    :function (x)
    _ nil))

(defn second [x]
  "Returns the second item in `x` (any iterable), or nil if empty.

  Note: for iterators, consumes the first two values in x, returning the
  second."
  (match (type x)
    :table (if (callable? x) (do (x) (x)) (. x 2))
    :string (x:sub 2 2)
    :function (do (x) (x))
    _ nil))

(fn last-iter [it]
  (var prev nil)
  (fn step [...]
    (if (= nil ...)
      (if (= :table (type prev))
        (values (unpack prev 1 prev.n))
        prev)
      (match (select "#" ...)
        1 (do (set prev ...) (step (it)))
        n (do (set prev [...]) (tset prev :n n) (step (it))))))
  (step (it)))

(defn last [x]
  "Returns the last item in `x` (any iterable), or nil if empty.

  Note: for iterators, consumes all values in x and returns the last value."
  (match (type x)
    :table (if (callable? x) (last-iter x) (. x (length x)))
    :string (x:sub -1 -1)
    :function (last-iter x)
    _ nil))

(defn butlast [x ?n]
  "Returns everything but the last item in `x` (any iterable), or nil if empty.
  With `n`, returns everything but the last `n` items.

  Note: for iterators, same as (drop-last 1 x)."
  (let [n (or ?n 1)]
    (match (type x)
      :table (if (callable? x) (drop-last n x) [(unpack x 1 (- (length x) n))])
      :string (x:sub 1 (- (length x) n))
      :function (drop-last n x)
      _ nil)))

(fn nthrest-iter [it_ n]
  (var it it_)
  (for [i 1 n]
    (when (= nil (it))
      (do (set it nil-iter) (lua "do break end"))))
  it)

(defn nthrest [x n]
  "Returns `x` (any iterable) without the first `n` items, or nil if empty.

  Note: for iterators, consumes the first n value in x and returns an iterator
  over the remaining values. Essentially an eager version of (drop n x)."
  (match (type x)
    :table (if (callable? x) (nthrest-iter x 1) [(unpack x (+ n 1))])
    :string (x:sub (+ n 1))
    :function (nthrest-iter x n)
    _ nil))

(defn rest [x]
  "Returns `x` (any iterable) without the first item, or nil if empty.

  Note: for iterators, consumes the first value in x and returns an iterator
  over the remaining values. Essentially an eager version of (drop 1 x)."
  (nthrest x 1))

(defn ffirst [x]
  "Same as (first (first x))"
  (first (first x)))

(defn frest [x]
  "Same as (first (rest x))"
  (first (rest x)))

(defn rfirst [x]
  "Same as (rest (first x))"
  (rest (first x)))

(defn llast [x]
  "Same as (last (last x))"
  (last (last x)))


;;; -- Iterator composition ---------------------------------------------------

(defn zip [...]
  "Zips multiple iterables together. Stops with the shortest iterable. Roughly
  equivalent to python's zip(...), or clojure's (map vector ...). Only supports
  single value iterators."
  (match (select "#" ...)
    1 (iter ...)
    2 (let [(xs_ ys_) ...
            xs (iter xs_)
            ys (iter ys_)]
        (fn []
          (let [x (xs) y (ys)]
            (when (not= nil y)
              (values x y)))))
    3 (let [(xs_ ys_ zs_) ...
            xs (iter xs_)
            ys (iter ys_)
            zs (iter zs_)]
        (fn []
          (let [x (xs) y (ys) z (zs)]
            (when (and (not= nil y) (not= nil z))
              (values x y z)))))
    n (let [its (mapt iter [...])]
        (fn []
          (let [vals (mapt #($) its)]
            (for [i 1 n]
              (when (= nil (. vals i))
                (lua "do return nil end")))
            (unpack vals))))))

(defn enumerate [...]
  "Zips multiple iterables together, prepending the index to each return.
  Essentially (zip (range) ...)."
  (if (and (= :table (type ...)) (= 1 (select "#" ...)))
    (iter-indexed ...)
    (zip (range) ...)))

(defn concat [...]
  "Iterates through any number of iterables, in order."
  (if (= nil ...)
    nil-iter
    (let [its [...]]
      (var i 1)
      (var it (iter (. its 1)))
      (fn step [...]
        (if (not= nil ...)
          ...
          (do
            (set i (+ i 1))
            (match (. its i)
              it_ (do (set it it_) (step (it)))))))
      #(step (it)))))

(defn cycle [iterable]
  "Repeatedly iterates through all values in `iterable`."
  (let [orig (iter-cached iterable)]
    (var it (orig:copy))
    (fn step [...]
      (if (not= nil ...)
        ...
        (do
          (set it (orig:copy))
          (step (it)))))
    #(step (it))))

(declare catv)

(defn interleave [...]
  "Iterates over the first item in each iterable, then the second, etc. Stops
  at the shortest iterable."
  (catv (zip ...)))

(defn interpose [sep iterable]
  "Iterates over items in `iterable`, adding `sep` between each item. Only
  supports single-value iterators."
  (var it (iter iterable))
  (var pending nil)
  (var sep? false)
  (fn []
    (if (and sep? pending)
      (do (set sep? false) sep)
      (match (or pending (it))
        x (do (set pending (it)) (set sep? true) x)
        _ (do (set it nil-iter) nil)))))



;;; -- Traversal (map, filter) ------------------------------------------------

(fn map-step [f ...]
  (when (not= nil ...) (f ...)))

(defn map [f ...]
  "Maps `f` over any number of `iterables`. `f` should take as many arguments
  as iterables."
  (let [it (zip ...)]
    #(map-step f (it))))

(fn filter-step [pred it ...]
  (when (not= nil ...)
    (if (pred ...)
      ...
      (filter-step pred it (it)))))

(defn filter [pred iterable]
  "Filters `iterable` to keep only values where `pred` returns truthy."
  (let [it (iter iterable)]
    #(filter-step pred it (it))))

(defn remove [pred iterable]
  "Filters `iterable` to remove any values where `pred` returns truthy."
  (filter (complement pred) iterable))

(var keep-step2 nil)
(var keep-step (fn [f it ...]
                 (when (not= nil ...)
                   (keep-step2 f it (f ...)))))
(set keep-step2 (fn [f it ...]
                  (if ...
                    ...
                    (keep-step f it (it)))))

(defn keep [f ...]
  "Maps `f` over any number of iterables, dropping any values where `f` returns
  falsey."
  (let [it (zip ...)]
    #(keep-step f it (it))))

(defn map-indexed [f ...]
  "Maps `f` over an index and any number of iterables.

  Essentially (map f (enumerate ...))."
  (let [it (enumerate ...)]
    #(map-step f (it))))

(defn keep-indexed [f ...]
  "Maps `f` over an index and any number of iterables, dropping any values
  where `f` returns falsey.

  Essentially (keep f (enumerate ...))."
  (let [it (enumerate ...)]
    #(keep-step f it (it))))


;;; -- Slicing (take, drop) ---------------------------------------------------

(defn take [n iterable]
  "Takes the first `n` items in `iterable`."
  (let [it (iter iterable)]
    (var i (math.max n 0))
    (fn []
      (when (not= 0 i)
        (set i (- i 1))
        (it)))))

(defn take-while [pred iterable]
  "Takes items from `iterable` while (pred item) returns truthy."
  (var it (iter iterable))
  (fn step [...]
    (if (and (not= nil ...) (pred ...))
      ...
      (set it nil-iter)))
  #(step (it)))

(defn take-upto [pred iterable]
  "Takes items from `iterable` up to and including the first item for which
  (pred item) returns truthy."
  (var it (iter iterable))
  (fn step [...]
    (if (and (not= nil ...) (pred ...))
      (do (set it nil-iter) ...)
      ...))
  #(step (it)))

(defn take-nth [n iterable]
  "Iterates over each `n` items in `iterable`."
  (let [it (iter iterable)]
    (var first? true)
    (fn []
      (if (not first?)
        (for [_ 2 n]
          (when (= nil (it))
            (lua "do return nil end")))
        (set first? false))
      (it))))

(defn drop [n iterable]
  "Drops the first `n` items in `iterable`, iterating over the remainder."
  (let [it (iter iterable)]
    (var i (math.max n 0))
    (fn loop []
      (if (= 0 i)
        (it)
        (do
          (set i (- i 1))
          (when (not= nil (it))
            (loop)))))))

(defn drop-while [pred iterable]
  "Drops items from `iterable` while (pred item) returns truthy."
  (let [it (iter iterable)]
    (var dropping? true)
    (fn step [...]
      (if dropping?
        (if (and (not= nil ...) (pred ...))
          (step (it))
          (do (set dropping? false) ...))
        ...))
    #(step (it))))

(defn drop-upto [pred iterable]
  "Drops items from `iterable` up to and including the first item for which
  (pred item) returns truthy."
  (let [it (iter iterable)]
    (var dropping? true)
    (fn step [...]
      (if (and dropping? (not= nil ...) (pred ...))
        (do (set dropping? false) (step (it)))
        ...))
    #(step (it))))

(defn drop-last [n iterable]
  "Drops the last `n` items in iterable. In other words, _takes_ all but the
  last `n` items."
  (let [head (iter-cached iterable)
        tail (drop n (head:copy))]
    (map #$1 head tail)))

(defn take-last [n iterable]
  "Takes only the last `n` items in iterable. In other words,  _drops_ all but
  the last `n` items. Evaluates the entire iterable on the first iteration."
  (let [it (iter-cached iterable)]
    (var first? true)
    (fn []
      (when first?
        (set first? false)
        ;; run through the whole thing and count how many items there are
        (var count 0)
        (each [_ (it:copy)] (set count (+ count 1)))
        ;; drop all but the last `n`
        (for [_ 1 (- count n)] (it)))
      (it))))


;;; -- Deduplication ----------------------------------------------------------

(defn distinct [iterable]
  "Filters `iterable` so that it only contains distinct items. Only supports
  single-value iterators.

  Note: values must be table keys. See [[distinct-by]] if you need to use
  multi-value iterators, or for custom equality (e.g. if you need to compare
  tables)."
  (let [it (iter iterable)]
    (var seen {})
    (fn loop []
      (match (it)
        x (if (. seen x)
            (loop)
            (do (tset seen x true)) x)))))

(fn distinct-by-step [f seen it ...]
  (when (not= nil ...)
    (let [key (f ...)]
      (if (. seen key)
        (distinct-by-step f seen it (it))
        (do (tset seen key true) ...)))))

(defn distinct-by [f iterable]
  "Filters `iterable` so that it only contains items that are distinct when
  passed to `f`. Supports multi-value iterators.

  NB: `f` must return values that can be used as table keys."
  (let [it (iter iterable)
        seen {}]
    #(distinct-by-step f seen it (it))))


;;; -- Grouping and flattening (partition, cat) -------------------------------

;; Note: grouping functions that have to traverse the whole collection are in
;; the "reducing" section (e.g. group-by, frequencies).

(fn partition-table [all? n step pad tbl]
  (let [len (length tbl)
        stop (if
               all? len
               pad (clamp (+ len step (- n) 1) 1 len)
               :else (+ len (- n) 1))]
    (var i 1)
    (fn []
      (when (<= i stop)
        (let [ret [(unpack tbl i (+ i (- n 1)))]]
          (when (and pad (= nil (. ret n)))
            (for [i 1 (- n (length ret))]
              (tinsert ret (. pad i))))
          (set i (+ i step))
          ret)))))

(fn skip-part! [n it]
  (for [_ 1 n]
    (when (= nil (it))
      (lua "do return end")))
  true)

(fn fill-part! [tbl n it]
  (for [i 1 (- n (length tbl))]
    (match (it)
      x (tinsert tbl x)
      _ (lua "do break end")))
  tbl)

(fn partition-iter [all? n step pad iterable]
  (let [gap (when (< n step) (- step n))]
    (var it (iter iterable))
    (var first? true)
    (var part []) ; next partition
    (fn []
      (when (or (not gap) first? (skip-part! gap it))
        (when first? (set first? false))
        (let [ret (fill-part! part n it)]
          (when (not= nil (. ret 1))
            ;; take any overlap for the next iteration
            (set part [(unpack ret (+ step 1))])
            (if (not= nil (. ret n))
              ;; normal return
              ret
              ;; otherwise we've exhausted the iterator
              (do
                (set it nil-iter)
                (when pad
                  (for [i 1 (math.min (length pad) (- n (length ret)))]
                    (tinsert ret (. pad i))))
                (if
                  ;; partition-all includes all non-full iterations
                  all? ret
                  ;; partition with pad just pads the final non-full iteration
                  pad (do (set part []) ret)
                  ;; partition without pad skips the final non-full iteration
                  nil)))))))))

(fn partition-impl [all? ...]
  (match (select "#" ...)
    2 (let [(n iterable) ...]
        (partition-impl all? n n nil iterable))
    3 (let [(n step iterable) ...]
        (partition-impl all? n step nil iterable))
    4 (let [(n step pad iterable) ...]
        (if
          (< n 0) nil-iter
          (array? iterable) (partition-table all? n step pad iterable)
          :else (partition-iter all? n step pad iterable)))))

(defn partition [...]
  "(partition n iterable)
  (partition n step iterable)
  (partition n step pad iterable)

  Partitions `iterable` into tables, each containing `n` elements. The start of
  each table is separated by `step` iterations.  Without `step`, defaults to
  separating by `n` iterations, i.e. without any overlap or gap.

  Without `pad`, skips the final group if it contains < `n` elements.

  With `pad` (a table), pads the final group with that table. Does not skip the
  final group even if it contains < `n` elements after padding.

  Only supports single-value iterators. For multi-value iterators, you may want
  to use [[catv]] to flatten multivals before calling partition."
  (or (partition-impl false ...)
      (error "partition: expected 2, 3, or 4 args")))

(defn partition-all [...]
  "(partition-all n iterable)
  (partition-all n step iterable)
  (partition-all n step pad iterable)

  Like [[partition]], but includes any final groups with < `n` elements. With
  step < n there may be multiple such final groups."
  (or (partition-impl true ...)
      (error "partition-all: expected 2, 3, or 4 args")))

(defn partition-when [f iterable]
  "Partitions `iterable` into tables, splitting each time `f` returns truthy.
  Only supports single-value iterators."
  (var it (iter iterable))
  (var pending [])
  (fn loop []
    (let [x (it)]
      (if
        ;; we're at the end: return any remaining pending items
        (= nil x)
        (when (not= nil (. pending 1))
          (let [ret pending]
            (set it nil-iter)
            (set pending [])
            ret))
        ;; we hit a split (and we have a pending group)
        (and (not= nil (. pending 1)) (f x))
        (let [ret pending]
          (set pending [x]) ; start the next group
          ret)
        ;; no split, keep accumulating
        (do (tinsert pending x) (loop))))))

(defn partition-by [f iterable]
  "Partitions `iterable` into tables, splitting each time `f` returns a
  different value. Only supports single-value iterators."
  (var prev {}) ; a unique value to start
  (partition-when (fn [...]
                    (let [key (f ...)]
                      (if (not= key prev)
                        (do (set prev key) true))))
                  iterable))

(defn cat [iterable]
  "Takes an `iterable` that produces other iterable values, and flattens them
  into the output."
  (let [it (iter iterable)]
    (var pending nil-iter)
    (fn step [...]
      (if (not= nil ...)
        ...
        (match (it)
          xs (do
               (set pending (iter xs))
               (step (pending))))))
    #(step (pending))))

(fn catv-fill-pending [pending fst ...]
  (when (not= nil fst)
    (let [count (select "#" ...)]
      (for [i 1 count]
        (tset pending i (select i ...)))
      ;; we're reusing `pending`, so mark the end with a nil
      (tset pending (+ count 1) nil))
    ;; the first value is returned immediately instead of being added to
    ;; pending
    fst))

(defn catv [iterable]
  "Takes an `iterable` that produces multiple values, and flattens them into
  the output (i.e. iterates over one value at a time)."
  (let [it (iter iterable)
        pending []]
    (var i 0)
    (fn []
      (set i (+ i 1))
      (match (. pending i)
        x x
        _ (do (set i 0) (catv-fill-pending pending (it)))))))

(defn mapcat [f ...]
  "Maps `f` over any number of iterables. `f` should return another iterable
  (e.g. a table), which will be flattened into the output using [[cat]].

  See also [[mapcatv]] to use multiple return values, which can be
  significantly more efficient than returning a table."
  (cat (map f ...)))

(defn mapcatv [f ...]
  "Maps `f` over any number of iterables. `f` may return multiple values, which
  will be flattened into the output using [[catv]]."
  (catv (map f ...)))


;;; -- Reducing ---------------------------------------------------------------

(fn reduce-step [f acc ...]
  (if (not= nil ...)
    (values true (f acc ...))
    (values nil acc)))

(defn reduce [f ...]
  "(reduce f init iterable)
  (reduce f iterable) -- uses the first value of `iterable` in place of `init`

  Repeatedly calls (f result x) for each value in `iterable`, returning the
  result once `iterable` is exhausted. If `iterable` returns multiple values,
  `f` is passed all of them."
  (match ...
    ;; 3-arg reduce
    (?init iterable) (let [it (iter iterable)]
                       (var (continue? acc) (values true ?init))
                       (while continue?
                         (set (continue? acc) (reduce-step f acc (it))))
                       acc)
    ;; 2-arg reduce
    (iterable) (let [it (iter iterable)]
                 ;; (destructively) take the first item as the initial value
                 (reduce f (it) it))))

(defn reductions [f ...]
  "(reductions f init iterable)
  (reductions f iterable)

  Iterates over successive steps of reducing `f` over `init` and `iterable`."
  (match ...
    (?init iterable) (let [it (iter iterable)]
                       (var (first? continue? acc) (values true true nil))
                       (fn []
                         (if first?
                           (do (set first? false) (set acc ?init) acc)
                           (match (reduce-step f acc (it))
                             (true x) (do (set acc x) x)))))
    (iterable) (let [it (iter iterable)]
                 (var (first? continue? acc) (values true true nil))
                 (fn []
                   (if first?
                     (do (set first? false) (set acc (it)) acc)
                     (match (reduce-step f acc (it))
                       (true x) (do (set acc x) x)))))))

(defn reduce-kv [f init tbl]
  "Calls (f result k v) for each key/value pair in `tbl`, returning result once
  `tbl` is exhausted."
  (var ret init)
  (each [k v (pairs tbl)]
    (set ret (f ret k v)))
  ret)

;; this mimics the syntax of fennel's accumulate
(macro accumulate* [[accum-var init binding iterable] ...]
  `(do
     (var ,accum-var ,init)
     (if (array? ,iterable)
       (for [i# 1 (length ,iterable)]
         (let [,binding (. ,iterable i#)]
           (set ,accum-var (do ,...))))
       (each [,binding (iter ,iterable)]
         (set ,accum-var (do ,...))))
     ,accum-var))

(defn run! [f iterable]
  "Calls `f` on each item in iterable.

  Equivalent to (each [x iterable] (f x)), but handles multiple values."
  (reduce (fn [_ ...] (f ...) nil) nil iterable))

(defn into! [tbl iterable]
  "Collects values from `iterable`, appending them to the end of `tbl`. Only
  supports single-value iterators. See [[into!+]] to use a multi-value
  iterator."
  (var end (length tbl))
  (accumulate* [tbl tbl
                x iterable]
    (set end (+ 1 end))
    (tset tbl end x)
    tbl))

(defn into!+ [tbl iterable]
  "Collects all values from `iterable`, appending each value to the end of
  `tbl`. Supports multi-value iterators."
  ;; a bit faster than table.insert since it caches the length
  (var end (length tbl))
  (fn step [tbl ...]
    (for [i 1 (select "#" ...)]
      (set end (+ 1 (or end (length tbl))))
      (tset tbl end (select i ...)))
    tbl)
  (reduce step tbl iterable))

(defn into-kv! [tbl iterable]
  "Collects key/value pairs from `iterable` into `tbl`."
  (each [k v (iter iterable)]
    (tset tbl k v))
  tbl)

(defn totable [iterable]
  "Collects each value returned by `iterable` into an array table. Only
  supports single-value iterators. See [[totable+]] for use with a multi-value
  iterator."
  (into! [] iterable))

(defn totable+ [iterable]
  "Collects each value returned by `iterable` into an array table. Supports
  multi-value iterators."
  (into!+ [] iterable))

(defn tomap [iterable]
  "Collects key/value pairs returned by `iterable` into a hash table."
  (into-kv! {} iterable))

(defn zipmap [keys vals]
  "Zips `keys` and `vals` iterators into a hash table."
  (into-kv! {} (zip keys vals)))

;; Reducing math functions

(defn sum [iterable]
  "Computes the sum of all values in `iterable`. Only supports single-value
  iterators."
  (accumulate* [total 0 x iterable] (+ total x)))

(defn product [iterable]
  "Computes the product of all values in `iterable`. Only supports single-value
  iterators."
  (accumulate* [total 1 x iterable] (* total x)))

(defn minimum [iterable]
  "Returns the minimum value in `iterable`. Items are compared with `<`. Only
  supports single-value iterators."
  (accumulate* [ret nil x iterable] (if (or (= nil ret) (< x ret)) x ret)))

(defn maximum [iterable]
  "Returns the maximum value in `iterable`. Items are compared with `<`. Only
  supports single-value iterators"
  (accumulate* [ret nil x iterable] (if (and (not= nil ret) (< x ret)) ret x)))

;; Reducing grouping functions

(defn frequencies [iterable]
  "Returns a table of {item count} for each item in `iterable`. Only supports
  single-value iterators."
  (accumulate* [ret {}
                x iterable]
    (tset ret x (+ 1 (or (. ret x) 0)))
    ret))

(defn group-by [f iterable]
  "Groups items in `iterable`, keyed by the result of calling `f` on each item.
  Each value is the group (table) of items in `iterable` with the corresponding
  key. In other words {(f x) [x etc...]}

  Only supports single-value iterators."
  (accumulate* [ret {}
                x iterable]
    (let [k (f x)]
      (match (. ret k)
        xs (tinsert xs x)
        _ (tset ret k [x]))
      ret)))

(defn index-by [f iterable]
  "Returns a map of the elements in `iterable`, keyed by the result of calling
  `f` on each item. Each value is the last item in `iterable` with the
  corresponding key. In other words, {(f x) x}

  Only supports single-value iterators."
  (accumulate* [ret {}
                x iterable]
    (tset ret (f x) x)
    ret))

;; Reducing predicates

(defn every? [pred iterable]
  "Returns true if all items in `iterable` satisfy `pred`."
  (not ((remove pred iterable))))

(defn not-every? [pred iterable]
  "Returns true if any of the items in `iterable` does not satisfy `pred`."
  (not (every? pred iterable)))

(defn any? [pred iterable]
  "Returns true if any of the items in `iterable` satisfy `pred`."
  (if ((filter pred iterable)) true false))

(defn not-any? [pred iterable]
  "Returns true if none of the items in `iterable` satisfy `pred`."
  (not (any? pred iterable)))

(defn find-first [pred iterable]
  "Returns the first item in `iterable` that satisfies `pred`."
  ((filter pred iterable)))

(defn some [f iterable]
  "Returns the first truthy value of (f item) for items in `iterable`."
  ((keep f iterable)))


;;; -- Iterator caching -------------------------------------------------------

;; Unlike Clojure's lazy-seq, which is cached by default, we only have one-shot
;; iterators. In particular, stateful iterators (string.gmatch, io.lines) can
;; only be one-shot iterators (stateless iterators could, in principle, be
;; started from anywhere, but we've chosen to wrap those as stateful iterators
;; for simplicity; see the comment at the top of the file).

;; Explicitly cached iterators bring caching and copying to all iterators, at
;; the expense of maintaining history in a table.

(var cached-table-meta nil)
(var cached-string-meta nil)
(var cached-fn-meta nil)

(set cached-iter?
     (fn [x]
       (match (getmetatable x)
         nil false
         cached-table-meta true
         cached-string-meta true
         cached-fn-meta true
         _ false)))

(fn cached-table [tbl]
  "Optimization for iterating over tables."
  (setmetatable {:table tbl :i 0} cached-table-meta))

(set cached-table-meta
     {:__call (fn [self]
                (set self.i (+ self.i 1))
                (. self.table self.i))
      :__index {:copy (fn [self]
                        (setmetatable {:table self.table :i self.i}
                                      cached-table-meta))}})

(fn cached-string [x]
  "Optimization for iterating over strings."
  (setmetatable {:string x :i 0 :end (length x)} cached-string-meta))

(set cached-string-meta
     {:__call (fn [self]
                (when (< self.i self.end)
                  (set self.i (+ self.i 1))
                  (self.string:sub self.i self.i)))
      :__index {:copy (fn [self]
                        (setmetatable {:string self.string :i self.i :end self.end}
                                      cached-string-meta))}})

(fn cached-fn [iterable]
  "A cache wrapper around a function-based iterator"
  (setmetatable {:it (iter iterable) :head {} :i 0} cached-fn-meta))

(let [vals-sentinel {}]
  ;; Think of this as a chunked linked list. Instead of `head` being a cons
  ;; cell of a single element, it's an array of up to 1024 elements, and `i` is
  ;; the current index within that chunk. Compared to a single array, this
  ;; allows garbage collection, since once we've moved past a chunk it's
  ;; garbage.  Compared to a non-chunked linked list it's reasonably
  ;; performant, since it doesn't have to allocate a new table for each item.
  (fn cached-fn-step [{: head &as self} ...]
    (let [n (select "#" ...)]
      (if
        (= nil ...) (set head.end? true)
        (<= n 1) (tset head self.i ...)
        (do
          ;; pack a multival like so: [vals-sentinel count v1 v2 ... vN]
          (var i self.i)
          (tset head i vals-sentinel)
          (set i (+ i 1))
          (tset head i n)
          (for [j 1 n]
            (set i (+ i 1))
            (tset head i (select j ...)))
          (set self.i i))))
    ...)

  (fn cached-fn-call [{: it &as self}]
    ;; 1. Increment
    (var i (+ self.i 1))
    (var head self.head)
    (when (<= 1024 i)
      ;; next cell if we've hit the end of this chunk
      (when (not head.next) (set head.next {}))
      (set self.head head.next)
      (set head self.head)
      (set i 1))
    ;; 2. Return
    (let [x (. head i)]
      (if
        ;; a multival packed like so: [vals-sentinel count v1 v2 ... vN]
        (= vals-sentinel x) (let [n (. head (+ i 1))
                                  start (+ i 2)
                                  stop  (+ i 1 n)]
                              (set self.i stop)
                              (values (unpack head start stop)))
        ;; a single value
        (not= nil x) (do (set self.i i) x)
        ;; nil, meaning we've run out of cached values and have to ask the
        ;; iterator for a fresh one
        (not head.end?) (do (set self.i i) (cached-fn-step self (it))))))

  (set cached-fn-meta
       {:__call cached-fn-call
        :__index {:copy (fn [{: it : head : i}]
                          (setmetatable {: it : head : i} cached-fn-meta))}}))

(defn iter-cached [iterable]
  "Returns a cached copy of an iterable.

  Cached iterators can be used transparently with functions in this module; if
  you want to use them outside this module (e.g. with generic for), you should
  call the `copy` method to get a fresh copy that starts from the beginning.

  (let [squares (iter-cached (map #(* $ $) (range 10)))]
    ;; functions from this module work fine:
    (sum squares) ; => 385

    ;; generic for requires making a copy:
    (var total 0)
    (each [x (squares:copy)]    ; <- must copy!
      (set total (+ total x)))
    total) ; => 385"
  (match (type iterable)
    :function (cached-fn (iter iterable))
    :string (cached-string iterable)
    :table (if
             (cached-iter? iterable) (iterable:copy)
             (callable? iterable) (cached-fn (iter iterable))
             :else (cached-table iterable))))

) ; end iterators


(comment
 (local {: require!} (require :bulb))
 (local b (require! :bulb))
 (local bi (require! :bulb.iter))
 (import-macros {: time-only} :bench)
 (bi.totable (bi.take 23 (bi.cycle (bi.concat [:a :b :c] [1 2]))))
 (->> (bi.range 1 100)
      ; (bi.map b.inc)
      (bi.partition 10)
      (bi.mapcat #(bi.map #(/ $ 100) $))
      (bi.sum)
      ; (bi.into [])
      )

 (bi.totable (bi.concat (bi.range :a :z) (bi.range :A :Z)))

 (bi.totable (bi.concat [:a :b :c] [1 2 3] "hello" (bi.range 10)))

 (bi.totable (bi.mapcat #(bi.totable (bi.range $)) (bi.range 10)))

 (bi.mapt #(values $ $ $) (bi.range 10))
 ;; vs
 (bi.totable (bi.mapcat #[$ $ $] (bi.range 10)))

 (bi.tomap (bi.map #(values $ $) [:a :b :c]))

 (let [rf (fn [m v]
            (tset m v (+ 1 (or (. m v) 0)))
            m)
       xs (bi.into [] (bi.take 100000 (bi.cycle [:a :b :c :d])))
       reduce-frequencies (fn [xs]
                            (bi.reduce rf {} xs))
       frequencies bi.frequencies]
   (print :frequencies (time-only
                        (for [i 1 1000]
                          (frequencies xs))))
   (print :reduce (time-only
                   (for [i 1 1000]
                     (reduce-frequencies xs))))
   (print ((. (require :fennel) :view) (frequencies xs)))
   (print ((. (require :fennel) :view) (reduce-frequencies xs)))
   )
 (let [xs (bi.totable (bi.range 100000))]
   (print :reduce-all
          (time-only
           (for [i 1 1000]
             (bi.reduce-all (fn [_ a b] (+ a b)) nil xs xs))))
   (print :reduce
          (time-only
           (for [i 1 1000]
             (bi.reduce (fn [_ x] (+ x x)) nil xs xs))))
   )

 (let [xs (bi.totable (bi.range 10000))]
   (print :iter (time-only
                 (for [i 1 1000]
                   (each [x (bi.iter xs)]
                     x))))
   (print :ipairs (time-only
                   (for [i 1 1000]
                     (each [_ x (ipairs xs)]
                       x))))
   )

 (do
   (print :iterate (time-only
                    (for [i 1 1000]
                      (each [x (bi.take 1000 (bi.iterate #[(+ 1 (. $ 1))] [0]))]
                        x))))
   (print :iterate-vargs (time-only
                          (for [i 1 1000]
                            (each [x (bi.take 1000 (bi.iterate #(values $2 (+ $1 $2)) 0 1))]
                              x))))
   )
 )


;;;; Memory pressure situations
(comment
 (local I (require :bulb.iter))
 (length (I.totable (I.range 1e7))) ;; ok
 (length (I.totable (I.range 1e8))) ;; OOM
 (let [r (I.iter-cached (I.range 1e7))
       t (I.take 12 r)
       d (I.drop 12 r)]
   [(I.reduce #(+ $ 1) 0 t)
    (I.reduce #(+ $ 1) 0 d)])
 ; [12 9999988]

 ;; this actually works! it's ugly and it takes a while, but it works :)
 (do
   (var r (I.iter-cached (I.range 1e10)))
   ;; have to call copy manually since iter doesn't know about cached3
   (var t (I.take 12 (r:copy)))
   (var d (I.drop 12 (r:copy)))
   (set r nil)
   [(let [head-count (I.reduce #(+ $ 1) 0 t)]
      (set t nil) ; locals clearing
      (collectgarbage)
      head-count)
    (I.reduce #(+ $ 1) 0 (I.map-indexed
                          (fn [i x]
                            ;; seems like we have to force a gc.  not sure why
                            ;; lua wouldn't do this under memory pressure
                            (when (= 0 (% i 1000000))
                              (collectgarbage))
                            x) d))])

 ;; r holds on to the head since it stays in scope
 (let [r (I.iter-cached (I.range 1e8))
       t (I.take 12 r)
       d (I.drop 12 r)]
   [(I.reduce #(+ $ 1) 0 t)
    (I.reduce #(+ $ 1) 0 d)])
 ; PANIC: unprotected error in call to Lua API (not enough memory)

 )

B
