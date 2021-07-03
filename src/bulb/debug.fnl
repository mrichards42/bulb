;; debug helpers

(fn locals [?n]
  "Returns a table with all locals, of the form [{: name : value} ...]."
  (let [ret {}]
    (for [i 1 500]
      (match (debug.getlocal (+ 1 (or ?n 1)) i)
        (name value) (table.insert ret {: name : value})
        _ (lua "do break end")))
    ret))

(fn local-names [?n]
  (icollect [_ v (ipairs (locals ?n))]
    v.name))

(fn upvalues [?f]
  "Returns a table with all upvalues, of the form [{: name : value} ...]."
  (let [ret {}
        f (if (= :function (type ?f))
            ?f
            (. (debug.getinfo (+ 1 (or ?f 1)) :f) :func))]
    (for [i 1 500]
      (match (debug.getupvalue f i)
        (name value) (table.insert ret {: name : value})
        _ (lua "do break end")))
    ret))

(fn upvalue-names [?f]
  "Returns the name of all upvalues."
  (icollect [_ v (ipairs (upvalues ?f))]
    v.name))

{: locals
 : local-names
 : upvalues
 : upvalue-names}
