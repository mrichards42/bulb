(local lu (require :test.luaunit))
(local B (require :bulb))
(import-macros {: icollect*} :bulb)

(local test-modules
  [:test.predicates
   :test.iterators])

(let [runner (doto (lu.LuaUnit.new)
                   (: :setOutputType :tap))
      tests (icollect* [(_ mod) (ipairs test-modules)
                        (k v) (pairs (require mod))]
              [(.. mod "." k) v])]
  (os.exit (runner:runSuiteByInstances tests)))
