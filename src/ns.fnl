(fn map-args [f ...]
  (unpack (icollect [_ x (ipairs [...])]
            (f x))))

;;; require-like macros

(fn all-defs [mod-name]
  (collect [k v (pairs (require mod-name))]
    (when (not (string.match k "^_"))
      (values k (sym k)))))

(fn require-all [mod-name ...]
  "Requires a module and locally binds all public symbols from the module."
  `(values
    ,(map-args (fn [mod] `(local ,(all-defs mod) (require ,mod)))
               mod-name ...)))

(fn require* [binding1 module-name1 ...]
  "Like require, but also binds the return value to a symbol, similar to
  `import-macros`. Supports destructuring."
  (assert (= 0 (% (select "#" binding1 module-name1 ...) 2))
          "expected an even number of binding/module-name pairs")
  (let [ret []]
    (for [i 1 (+ 2 (select "#" ...)) 2]
      (let [(name mod) (select i binding1 module-name1 ...)]
        (table.insert ret `(local ,name (require ,mod)))))
    `(values ,(unpack ret))))


;;; ns and exporter macros

(fn ns [name ?doc ...]
  "Clojure-like namespace form. Binds *ns* as the module table.

  Supports the following clauses:

  (:require binding :module ...)
  (:require-all :module ...)
  (:import-macros binding :module ...)
  (:require-macros :module ...)"
  (let [docstring (when (= :string (type ?doc)) ?doc)]
    (fn strip-values [x]
      (if (and (list? x) (= `values (. x 1)))
        (unpack x 2)
        x))
    `(values
      ;; define the *ns* local
      (local ,(sym :*ns*) {:_NAME ,(tostring name)
                           :_DOC ,docstring})
      ;; handle all the clauses
      ,(map-args
        (fn [form]
          (match (when (list? form) form)
            [:require & args] (strip-values (require* (unpack args)))
            [:import-macros & args] `(import-macros ,(unpack args))
            [:require-macros & args] (mapva #`(require-macros ,$) (unpack args))
            [:require-all & args] (strip-values (require-all (unpack args)))
            _ (error (.. "Unknown ns clause: " (view form)) 1)))
        (if docstring ... (values ?doc ...))))))

(fn ns-export []
  "Returns the namespace table. Must be the last form in the file."
  (sym :*ns*))

(fn ns-export-macros []
  "Returns only functions from the namespace table. Must be the last form in
  the file."
  `(collect [k# v# (pairs ,(sym :*ns*))]
     (when (= :function (type v#))
       (values k# v#))))


;;; def/defn/declare

(fn declare [name1 ...]
  (let [declares []]
    `(values ,(unpack (icollect [_ name (ipairs [name1 ...])]
                        `(var ,name nil))))))

(fn def [name ?doc val] ; ?doc is ignored since fennel only has fn metadata
  "Defines a local and adds it to the *ns* table.

  Use [[local]] or [[var]] to define a private variable."
  (if (not val)
    (def name nil ?doc)
    (if (in-scope? name)
      `(values (set-forcibly! ,name ,val)
               (tset ,(sym :*ns*) ,(tostring name) ,name))
      `(values (local ,name ,val)
               (tset ,(sym :*ns*) ,(tostring name) ,name)))))

(fn defn [name ...]
  "Defines a function and adds it to the *ns* table.

  Use [[fn]] to define a private function."
  (if (in-scope? name)
    `(values (set-forcibly! ,name (fn ,name ,...))
             (tset ,(sym :*ns*) ,(tostring name) ,name))
    `(values (fn ,name ,...)
             (tset ,(sym :*ns*) ,(tostring name) ,name))))

{: ns
 : require*
 : require-all
 : declare
 : def
 : defn
 : ns-export
 : ns-export-macros}
