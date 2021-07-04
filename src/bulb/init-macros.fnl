;;;; A few each-like macros

(fn binding? [bind]
  (or (sym? bind)
      (varg? bind)
      (and (or (table? bind) (list? bind))
           (let [(_ v) (next bind)]
             (sym? v)))))

;; neat, but probably not going to do it since it's pretty inefficient
(fn let-varg [[bind expr & more-bindings] ...]
  "Let, but allows binding to the varg symbol (...)."
  (if
    ;; body
    (= nil bind) ...
    ;; literal ...
    (= `... bind) `((fn [...] ,(let-varg more-bindings ...)) ,expr)
    ;; ... in a varg destructure
    (and (list? bind) (= `... (. bind (length bind))))
    `((fn [,(unpack bind)] ,(let-varg more-bindings ...)) ,expr)
    :else
    `(let [,bind ,expr] ,(let-varg more-bindings ...))))

;; the original version using bulb.iter.cat
(fn each-iter-old [bindings body ...]
  "Like [[doseq]], but returns an iterator instead of evaluating immediately.
  Essentially the same as Clojure's `for`.

  Supports multiple iterators, as well as :let, :when, and :while clauses."
  (fn inner-iterator? [more-bindings]
    (for [i 1 (length more-bindings) 2]
      (when (binding? (. more-bindings i))
        (lua "return false")))
    true)
  (fn new-syms []
    {:loop (gensym :loop) :it (gensym :it) :state (gensym :state) :ctrl (gensym :ctrl)})
  (fn parse-1 [syms [fst snd & more-bindings] body]
    (match (values fst snd)
      (nil nil) body
      ;; modifiers -- note that :when and :while have different 'else'
      ;; conditions than in doseq, since `nil` halts iteration
      (:let let-bindings) `(let ,let-bindings ,(parse-1 syms more-bindings body))
      (:when test) `(if ,test ,(parse-1 syms more-bindings body) (,syms.loop))
      (:while test) `(when ,test ,(parse-1 syms more-bindings body))
      (:until test) `(when (not ,test) ,(parse-1 syms more-bindings body))
      ;; iterator
      (where (bind* expr) (binding? bind*))
      (let [user-bindings (if (list? bind*) bind* (list bind*))
            {: it : state : ctrl : loop &as syms} (new-syms)
            bind-gen (icollect [i s (ipairs user-bindings)]
                       (when (< 1 i) ; don't need one for the ctrl sym
                         (gensym :bind)))]
        `(do
           (var (,it ,state ,ctrl ,(unpack bind-gen)) ,expr)
           (->> (fn ,loop []
                  (set (,ctrl ,(unpack bind-gen)) (,it ,state ,ctrl))
                  (when (not= nil ,ctrl)
                    (let [,user-bindings (values ,ctrl ,(unpack bind-gen))]
                      ,(parse-1 syms more-bindings body))))
                ,(if (not (inner-iterator? more-bindings))
                   ;; TODO: inline this?
                   `((. (require :bulb.iter) :cat))))))
      _ (error (.. "each-iter: expected a binding (symbol, list, or table) or "
                   ":let, :while, or :when but saw " (view [fst snd])))))
  (assert (= nil ...) "each-iter: one body form allowed")
  (assert (not= nil body) "each-iter: body for is required")
  (assert (binding? (. bindings 1)) "each-iter: first binding must be to an iterator")
  (parse-1 nil bindings body))

;;; The new version, using tail calls like goto, without using cat at all

(fn partition-bindings [bindings]
  ;; partition bindings so that each group only includes one iterator binding
  ;; (and any number of modifier bindings)
  (let [groups []]
    (each [i x (ipairs bindings)]
      (if (and (not= 0 (% i 2)) (binding? x))
        (table.insert groups [x])
        (table.insert (. groups (length groups)) x)))
    groups))

;; See the end of this file for example macroexpansions
(fn each-iter-1 [inner outer [[bind expr & modifiers] & inner-groups] body]
  (let [it (gensym :it) state (gensym :state) ctrl (gensym :ctrl)
        loop (gensym :loop)
        outer-call (if outer `(,outer) `nil)
        do-mods (fn do-mods [[fst snd & more-mods]]
                  (match (values fst snd)
                    (nil nil) (if (next inner-groups)
                                (each-iter-1 inner loop inner-groups body)
                                body)
                    (:let let-bindings) `(let ,let-bindings ,(do-mods more-mods))
                    (:when test) `(if ,test ,(do-mods more-mods) (,loop))
                    (:while test) `(if ,test ,(do-mods more-mods) ,outer-call)
                    (:until test) `(if (not ,test) ,(do-mods more-mods) ,outer-call)
                    _ (error (.. "each-iter: expected :let, :while, :when, or :until, "
                                 "but saw " (view [fst snd])))))
        user-bindings (if (list? bind) bind (list bind))
        bind-gen (icollect [i s (ipairs user-bindings)]
                   (when (< 1 i) ; first binding is ctrl
                     (gensym :bind)))
        init-vars `(var (,it ,state ,ctrl ,(unpack bind-gen)) ,expr)
        declare-fn `(fn ,loop []
                      (set (,ctrl ,(unpack bind-gen)) (,it ,state ,ctrl))
                      (if (not= nil ,ctrl)
                        (let [,user-bindings (values ,ctrl ,(unpack bind-gen))]
                          ,(do-mods modifiers))
                        ,outer-call))]
    (match (values outer (next inner-groups))
      ;; This case means it's a single-level (not nested) iterator, so there
      ;; are no chains of iterators to set in action. All we need to do is
      ;; return the simple iterator function
      (nil nil) `(do ,init-vars ,declare-fn ,loop)
      ;; Outermost nested interator -- setup the initial chain
      (nil _) `(do (var ,inner nil)
                 ,init-vars ,declare-fn
                 ;; the first call kicks off a chain that starts at the
                 ;; outermost iterator and ends at the innermost iterator,
                 ;; eventually rebinding `inner` (see the next case)
                 (set ,inner ,loop)
                 (fn [] (,inner)))
      ;; Innermost nested iterator -- rebind `inner`
      (_ nil) `(do ,init-vars ,declare-fn
                 (set ,inner ,loop)
                 (,loop))
      ;; Otherwise this is an intermediary iterator, so keep the chain going
      _ `(do ,init-vars ,declare-fn
           (,loop)))))

(fn each-iter [bindings body ...]
  "Like [[doseq]], but returns an iterator instead of evaluating immediately.
  Essentially the same as Clojure's `for`.

  Supports multiple iterators, as well as :let, :when, :while, and :until
  clauses."
  (assert-compile (= nil ...) "each-iter: one body form allowed" [])
  (assert-compile (not= nil body) "each-iter: body form is required" [])
  (assert-compile (= 0 (% (length bindings) 2))
                  "each-iter: expected an even number of bindings" [])
  (assert-compile (binding? (. bindings 1))
                  "each-iter: first binding must be to an iterator" [])
  (each-iter-1 (gensym :inner) nil (partition-bindings bindings) body))

(fn doseq [bindings ...]
  "Like [[each]], but supports multiple iterators, and adds support for :let,
  :when, :while, and :until clauses. Essentially the same as Clojure's `doseq`.

  Note: multiple returns must be destructured as in [[let]], e.g.
  (doseq [(k v) (pairs tbl)] ...) rather than (each [k v (pairs tbl)] ...)"
  (fn doseq-1 [[fst snd & more-bindings] ...]
    (match (values fst snd)
      (nil nil) ...
      ;; modifiers
      (:let let-bindings) `(let ,let-bindings ,(doseq-1 more-bindings ...))
      (:when test) `(when ,test ,(doseq-1 more-bindings ...))
      (:while test) `(if ,test ,(doseq-1 more-bindings ...) (lua "do break end"))
      (:until test) `(if ,test (lua "do break end") ,(doseq-1 more-bindings ...))
      ;; iterator
      (where (bind* expr) (binding? bind*))
      (let [each-forms (if (list? bind*) [(unpack bind*)] [bind*])]
        (table.insert each-forms expr)
        `(each [,(unpack each-forms)]
           ,(doseq-1 more-bindings ...)))
      _ (error (.. "doseq: expected a binding (symbol, list, or table) or "
                   ":let, :while, or :when but saw " (view [fst snd])))))
  (assert (binding? (. bindings 1)) "doseq: first binding must be to an iterator")
  (doseq-1 bindings ...))

;; maybe doiter, coiter, doseq?
;; or each-iter coroutine-iter, each+
(fn coroutine-iter [bindings body ...]
  "Like [[each-iter]], but using a coroutine. More efficient for deeply nested
  iterators, or if there is a `:when` condition that is rarely satisfied."
  `(coroutine.wrap
    (fn []
      ,(doseq bindings `(coroutine.yield ,body)))))

; ;; multi-iterator-aware collect and icollect versions

(fn collect* [bindings body]
  "Like [[collect]], but with syntax like [[doseq]]; that is:
  * multi-values must be wrapped in ()
  * supports nested iterators
  * supports :let, :when, and :while"
  `(let [tbl# {}]
     ,(doseq bindings
             `(let [(k# v#) ,body]
                (tset tbl# k# v#)))
     tbl#))

(fn icollect* [bindings body]
  "Like [[icollect]], but with syntax like [[doseq]]; that is:
  * multi-values must be wrapped in ()
  * supports nested iterators
  * supports :let, :when, and :while"
  `(let [tbl# {}]
     (var i# 0)
     ,(doseq bindings
             `(match ,body
                v# (do
                     (set i# (+ i# 1))
                     (tset tbl# i# v#))))
     tbl#))


(comment

 (require-macros :bulb.each-iter)

 (macrodebug (let-varg [a 1 b 2] (+ a b)))
 (macrodebug (let-varg [a 1
                        ... (values 2 3 4)
                        (x ...) ...]
               (+ a x (select "#" ...))))

 (fn map [f iter]
   (each-iter [... iter]
              (f ...)))
 
 (fn filter [pred ...]
   (macrodebug
   (each-iter [... ...
               :when (pred ...)]
              ...)))

 (fn keep [f iter]
   (each-iter [... iter
               :let [... (f ...)]
               :when (not= nil ...)]
              ...))

 (fn take-while [pred iter]
   (each-iter [... iter
               :while (pred ...)]
              ...))

 (fn concat [...]
   (each-iter [(_ iterable) (ipairs [...])
               ... (iter iterable)]
              ...))
 (fn cat [iter]
   (each-iter [it iter
               ... it]
              ...))
 (fn mapcat [f iter]
   (each-iter [... iter
               :let [it (f ...)]
               ... it]
              ...))

 (fn range [stop]
   (var i 0)
   (fn []
     (when (< i stop)
       (set i (+ i 1))
       i)))

 (fn pythagorean-triples [max-side]
   (each-iter [c (range max-side)
               b (range c)
               a (range b)
               :let [a2 (* a a)
                     b2 (* b b)
                     c2 (* c c)]
               :when (= (+ a2 b2) c2)]
              (values a b c)))

 (each [a b c (pythagorean-triples 50)]
   (print a b c))
 ; 3       4       5
 ; 6       8       10
 ; 5       12      13
 ; 9       12      15
 ; 8       15      17
 ; 12      16      20
 ; 15      20      25
 ; 7       24      25
 ; 10      24      26
 ; 20      21      29
 ; 18      24      30
 ; 16      30      34
 ; 21      28      35
 ; 12      35      37
 ; 15      36      39
 ; 24      32      40
 ; 9       40      41
 ; 27      36      45
 ; 30      40      50
 ; 14      48      50
 )


;;; Annotated macroexpansions of each-iter

(comment
 ;; Single iterator
 (macrodebug
   (each-iter [(i x) (ipairs [1 2 3])]
      x))

 (do
   ;; Intiialize
   (var (it1 state1 ctrl1 bind1) (ipairs [1 2 3]))
   (fn loop1 []
     ;; Each step: call the iterator, save to temporary variables
     (set (ctrl1 bind1) (it1 state1 ctrl1))
     ;; Check for the end of iteration
     (if (not= nil ctrl1)
       ;; Now bind to (i x). This two-step binding is necessary in case table
       ;; destructuring is used, since destructuring on `nil` throws.
       (let [(i x) (values ctrl1 bind1)]
         x)
       nil))
   ;; Return the iterator function
   loop1)
 )

(comment
 ;; Nested iterators. The basic framework is the same as above, except for the
 ;; addition of an `inner` variable, which (aside from the initialization
 ;; phase) is always bound to the innermost iterator function.
 (macrodebug
   (each-iter [(_ x) (ipairs [1 2 3])
               (_ y) (ipairs (range x))
               (_ z) (ipairs (range y))]
     (values x y z)))

 (do
   (var inner nil)
   ;; init 1
   (var (it1 state1 ctrl1 bind1) (ipairs [1 2 3]))
   (fn loop1 []
     ;; (in 1) step 1
     (set (ctrl1 bind1) (it1 state1 ctrl1))
     (if (not= nil ctrl1)
       (let [(_ x) (values ctrl1 bind1)]
         (do
           ;; (in 1) init 2
           (var (it2 state2 ctrl2 bind2) (ipairs (range x)))
           (fn loop2 []
             ;; (in 2) step 2
             (set (ctrl2 bind2) (it2 state2 ctrl2))
             (if (not= nil ctrl2)
               (let [(_ y) (values ctrl2 bind2)]
                 (do
                   ;; (in 2) init 3
                   (var (it3 state3 ctrl3 bind3) (ipairs (range y)))
                   (fn loop3 []
                     ;; (in 3) step 3
                     (set (ctrl3 bind3) (it3 state3 ctrl3))
                     (if (not= nil ctrl3)
                       (let [(_ z) (values ctrl3 bind3)]
                         ;; (in 3) yield a value
                         (values x y z))
                       ;; (in 3) end = get the next value from 2
                       (loop2)))
                   ;; (in 2) now that 3 is set up, rebind inner and call it,
                   ;; which will yield a new (values x y z)
                   (set inner loop3)
                   (loop3)))
               ;; (in 2) end = get the next value from 1
               (loop1)))
           ;; (in 1) now that 2 is set up, call it, which will cascade down to
           ;; setting up and calling 3.
           (loop2)))
       ;; (in 1) end = we're done
       nil))
   ;; Outermost scope again -- in the initialization phase, inner is bound to
   ;; the outermost iterator. Calling it starts the following cascade:
   ;; * Call it1
   ;;    * Set up it2
   ;;    * Call it2
   ;;      * Set up it3
   ;;      * Bind inner <- it3
   ;;      * Call it3
   ;;        * Return values
   ;; After the initialization phase, inner is bound to it3, so on subsequent
   ;; iterations, we just call (inner)
   (set inner loop1)
   (fn [] (inner)))
 )

(comment
 ;; With modifiers
 (macrodebug
   (each-iter [(_ x) (ipairs [1 2 3])
               :when (= 0 (% x))
               :let [y (* x x)]
               :while (< y 10)]
      (values x y)))

 (do
   (var (it1 state1 ctrl1 bind1) (ipairs [1 2 3]))
   (fn loop1 []
     (set (ctrl1 bind1) (it1 state1 ctrl1))
     (if (not= nil ctrl1)
       (let [(_ x) (values ctrl1 bind1)]
         ;; when modifier
         (if (= 0 (% x))
           ;; let modifier
           (let [y (* x x)]
             ;; while modifier
             (if (< y 10)
               (values x y)
               ;; while false = end of iteration
               ;; if this were a nested iterator, this would call the parent
               ;; (outer) iterator instead of returning nil
               nil))
           ;; when false = try the next iteration
           (loop1)))
       nil))
   loop1)
  )


{: each-iter
 : coroutine-iter
 :each-iter2 each-iter-old
 : doseq
 : collect*
 : icollect*
 : let-varg}
