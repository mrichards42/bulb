(local M {})

;; Like clojure test

(fn M.is [assertion ?desc]
  `((. (require :test.luaunit) :assert_eval_to_true)
    ,assertion ,(or ?desc (tostring (view assertion)))))

;; Lispify luaunit assertions

(fn luaunit-assertion [assertion-name]
  (fn [expected code ?desc]
    `((. (require :test.luaunit) ,assertion-name)
      ,code ,expected ; luaunit expects reverse order by default
      ,(or ?desc (tostring (view code))))))

(comment
 (local assertion-fns (collect [k (pairs (require :test.luaunit))]
                        (when (k:match "^assert_")
                          (values (-> k (: :gsub "_" "-") (: :gsub "-equals" "="))
                                  k))))
 )

(local assertion-fns
  {:assert-almost= "assert_almost_equals"
   :assert-boolean "assert_boolean"
   :assert-coroutine "assert_coroutine"
   :assert-error "assert_error"
   :assert-error-msg-contains "assert_error_msg_contains"
   :assert-error-msg-content= "assert_error_msg_content_equals"
   :assert-error-msg-matches "assert_error_msg_matches"
   :assert-error-msg= "assert_error_msg_equals"
   :assert-eval-to-false "assert_eval_to_false"
   :assert-eval-to-true "assert_eval_to_true"
   :assert-false "assert_false"
   :assert-function "assert_function"
   :assert-inf "assert_inf"
   :assert-is "assert_is"
   :assert-is-boolean "assert_is_boolean"
   :assert-is-coroutine "assert_is_coroutine"
   :assert-is-false "assert_is_false"
   :assert-is-function "assert_is_function"
   :assert-is-inf "assert_is_inf"
   :assert-is-minus-inf "assert_is_minus_inf"
   :assert-is-minus-zero "assert_is_minus_zero"
   :assert-is-nan "assert_is_nan"
   :assert-is-nil "assert_is_nil"
   :assert-is-number "assert_is_number"
   :assert-is-plus-inf "assert_is_plus_inf"
   :assert-is-plus-zero "assert_is_plus_zero"
   :assert-is-string "assert_is_string"
   :assert-is-table "assert_is_table"
   :assert-is-thread "assert_is_thread"
   :assert-is-true "assert_is_true"
   :assert-is-userdata "assert_is_userdata"
   :assert-items= "assert_items_equals"
   :assert-minus-inf "assert_minus_inf"
   :assert-minus-zero "assert_minus_zero"
   :assert-nan "assert_nan"
   :assert-nil "assert_nil"
   :assert-not-almost= "assert_not_almost_equals"
   :assert-not-boolean "assert_not_boolean"
   :assert-not-coroutine "assert_not_coroutine"
   :assert-not-false "assert_not_false"
   :assert-not-function "assert_not_function"
   :assert-not-inf "assert_not_inf"
   :assert-not-is "assert_not_is"
   :assert-not-is-boolean "assert_not_is_boolean"
   :assert-not-is-coroutine "assert_not_is_coroutine"
   :assert-not-is-false "assert_not_is_false"
   :assert-not-is-function "assert_not_is_function"
   :assert-not-is-inf "assert_not_is_inf"
   :assert-not-is-nan "assert_not_is_nan"
   :assert-not-is-nil "assert_not_is_nil"
   :assert-not-is-number "assert_not_is_number"
   :assert-not-is-string "assert_not_is_string"
   :assert-not-is-table "assert_not_is_table"
   :assert-not-is-thread "assert_not_is_thread"
   :assert-not-is-true "assert_not_is_true"
   :assert-not-is-userdata "assert_not_is_userdata"
   :assert-not-minus-inf "assert_not_minus_inf"
   :assert-not-minus-zero "assert_not_minus_zero"
   :assert-not-nan "assert_not_nan"
   :assert-not-nil "assert_not_nil"
   :assert-not-number "assert_not_number"
   :assert-not-plus-inf "assert_not_plus_inf"
   :assert-not-plus-zero "assert_not_plus_zero"
   :assert-not-str-contains "assert_not_str_contains"
   :assert-not-str-icontains "assert_not_str_icontains"
   :assert-not-string "assert_not_string"
   :assert-not-table "assert_not_table"
   :assert-not-table-contains "assert_not_table_contains"
   :assert-not-thread "assert_not_thread"
   :assert-not-true "assert_not_true"
   :assert-not-userdata "assert_not_userdata"
   :assert-not= "assert_not_equals"
   :assert-number "assert_number"
   :assert-plus-inf "assert_plus_inf"
   :assert-plus-zero "assert_plus_zero"
   :assert-str-contains "assert_str_contains"
   :assert-str-icontains "assert_str_icontains"
   :assert-str-matches "assert_str_matches"
   :assert-string "assert_string"
   :assert-table "assert_table"
   :assert-table-contains "assert_table_contains"
   :assert-thread "assert_thread"
   :assert-true "assert_true"
   :assert-userdata "assert_userdata"
   :assert= "assert_equals"})

(each [k v (pairs assertion-fns)]
  (tset M k (luaunit-assertion v)))

M
