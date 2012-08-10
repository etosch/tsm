(ns tsm.test.core
  (:use [tsm.core]
	[tsm.instructions integer float])
  (:use [clojure.test]))

(deftest test-1
  (is (eval-tsm {:x ['integer_add 'integer_mult], :ts [], :integer [1 2 3]} :direction :lr)
      {:x [], :ts [], :integer [9]}))

(deftest test-2
  (is (eval-tsm {:x [{:imap 'ts_tag :tag 0.5} 27], :ts [{}]} :direction :lr)
      {:x [], :ts [{0.5 27}]}))

(deftest test-3
  (is (eval-tsm {:x [{:imap 'ts_tagged :tag 10.0}], :ts [{10.0 "foo"}]
		 :string ["bar"]})
      {:x [], :ts [{10.0 ["foo" "bar"]}]
       :string ["bar" "foo" "baz"]}))

(deftest test-4
  (is (eval-tsm {:x [{:imap 'ts_tagged :tag 10.0}], :ts [{10.0 "foo"}]
		 :string ["bar"]} :direction :lr)
      {:x [], :ts [{10.0 ["foo" "bar"]}]
       :string (vec (reverse ["bar" "foo" "baz"]))}))

(deftest test-5
  (is (eval-tsm {:x [{:imap 'ts_tagged :tag 10.0}], :ts [{0.5 27, 23.0 'integer_add}]
		 :integer [1 2]})
      {:x [], :ts [{0.5 27, 23.0 'integer_add}]
       :integer [3]}))

(deftest test-6
  (is ((eval-tsm {:x [{:imap 'ts_tagged :tag 0.0}],
		  :ts [{0.0 [0.3 {:imap 'ts_tagged :tag 1.0}],
			1.0 [0.2 {:imap 'ts_tagged :tag 2.0}],
			2.0 [0.1 {:imap 'ts_tagged :tag 3.0}],
			3.0 ['float_add 'float_mult]}]})
       :float)
      0.09))

(deftest test-7
  (is (eval-tsm {:integer [9] :x [{:imap 'integer_add_const :const 3}]})
      {:float [], :boolean [], :string [], :ts [{}], :x [], :integer [12]}))