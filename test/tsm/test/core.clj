(ns tsm.test.core
  (:use [tsm core config]
	[tsm.instructions integer ts float]
	[clojure.test]))

(reset! debug false)

(deftest test-1
  (is
   (= (eval-tsm {:x ['integer_add 'integer_mult], :integer [1 2 3]})
      (ensure-state {:integer [7]}))))

(deftest test-2
  (is (= (eval-tsm {:x [27 {:imap 'ts_tag :tag 0.5}]})
	 (ensure-state {:x [], :ts [{0.5 27}]}))))

(deftest test-3
   (is (= (eval-tsm {:x ["baz" {:imap 'ts_tagged :tag 10.0}], :ts [{10.0 "foo"}] :string ["bar"]})
	  (ensure-state {:x [], :ts [{10.0 "foo"}] :string ["bar" "foo" "baz"]}))))

(deftest test-5
  (is (= (eval-tsm {:x [{:imap 'ts_tagged :tag 10.0}]
		    , :ts [(sorted-map 0.5 27 23.0 'integer_add)]
		    , :integer [1 2]})
	 (ensure-state {:x [], :ts [{0.5 27, 23.0 'integer_add}], :integer [3]}))))


(deftest test-6
  (let [finalstate (eval-tsm {:x [{:imap 'ts_tagged :tag 0.0}],
			       :ts [(sorted-map
				     0.0 [0.3 {:imap 'ts_tagged :tag 1.0}],
				     1.0 [0.2 {:imap 'ts_tagged :tag 2.0}],
				     2.0 [0.1 {:imap 'ts_tagged :tag 3.0}],
				     3.0 ['float_add 'float_mult])]})]
    (is (= (finalstate :float) [0.9]))))

(deftest test-add-const
  (is (= (eval-tsm {:integer [9] :x [{:imap 'integer_add_const :const 3}]})
	 (ensure-state {:integer [12]}))))

(deftest test-deep-stack-ts_tag
  (are [y x] (= x y)
       (eval-tsm {:x ["asdf" {:imap 'ts_tag :tag 12.3} 'ts_new 10 {:imap 'ts_tag :tag 1.0}]})
       (ensure-state {:ts [{1.0 10} {12.3 "asdf"}]})
       (eval-tsm {:x ["asdf" {:imap 'ts_tag :tag 12.3 :ith 2} 'ts_new 10 {:imap 'ts_tag :tag 1.0}]})
       (ensure-state {:ts [(sorted-map 1.0 10 12.3 "asdf") {}]})
       (eval-tsm {:x ["asdf" {:imap 'ts_tag :tag 3.1 :ith 3}] :ts [{}{}{}{}{}]})
       (ensure-state {:ts [{}{}{3.1 "asdf"}{}{}]})))

(deftest test-deep-stack-ts_tag_pair
  (are [x y] (= x y)
       (eval-tsm {:x [21 "asdf" {:imap 'ts_tag_pair :tag 12.3} 'ts_new 10 {:imap 'ts_tag :tag 1.0}]})
       (ensure-state {:ts [{1.0 10} {12.3 [21 "asdf"]}]})
       (eval-tsm {:x [21 "asdf" {:imap 'ts_tag_pair :tag 12.3 :ith 2} 'ts_new 10 {:imap 'ts_tag :tag 1.0}]})
       (ensure-state {:ts [(sorted-map 1.0 10 12.3 [21 "asdf"]) {}]})
       (eval-tsm {:x [21 "asdf" {:imap 'ts_tag_pair :tag 3.1 :ith 3}] :ts [{}{}{}{}{}]})
       (ensure-state {:ts [{}{}{3.1 [21 "asdf"]}{}{}]})))
