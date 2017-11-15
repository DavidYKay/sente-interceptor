(ns sinterceptor.core
  (:require [clojure.test :as t]
            [clojure.pprint :refer [pprint]]))

(def example-event [:event/type :argument {:other :args}])

(def context
  {:coeffects {:event example-event
               :db {}}
   :effects   {:db {:events [example-event]}
               :dispatch  [:event/other :param1]}
   :queue     []
   :stack     []})

(def db (atom {}))

(def example-interceptor
  {:id :db-context
   :before (fn [context] (assoc-in context [:coeffects :db] @db))
   :after (fn [context] (do (reset! db (-> context :effects :db))
                            context))})

(defn handle
  [[type &args :as event]]
  (if (= :event/type type)
    [:event/other :param1]
    [:event/error {:msg "Type mismatch"}]))

(def handler-interceptor
  {:id :handle-event
   :before (fn [context] (assoc-in context
                                   [:effects :dispatch]
                                   (handle (-> context :coeffects :event))))})

(def default-context
  {:coeffects {}
   :effects {}
   :queue [example-interceptor handler-interceptor]
   :stack []})

(defn context
  [event interceptors]
  (-> {}
      (assoc-in [:coeffects :event] event)
      (assoc :queue interceptors)))

(defn change-direction
  [context]
  (-> context
      (dissoc :queue)
      (assoc :queue (:stack context))))

(defn call-interceptor
  [context interceptor direction]
  (println "calling interceptor" (:id interceptor))
  (let [result (if-let [f (get interceptor direction)]
                 (f context)
                 context)]
    (pprint result)
    result))

(defn call-interceptors
  [context direction]
  (loop [context context]
    (let [queue (:queue context)]        ;; future interceptors
      (if (empty? queue)
        context
        (let [interceptor (peek queue)   ;; next interceptor to call
              stack (:stack context)]    ;; already completed interceptors
          (recur (-> context
                     (assoc :queue (pop queue)
                            :stack (conj stack interceptor))
                     (call-interceptor interceptor direction))))))))

(defn run
  [event]
  (let [context (context event [handler-interceptor example-interceptor ])]
    (-> context
        (call-interceptors :before)
        ((fn [c] (pprint c) c))
        change-direction
        ((fn [c] (pprint c) c))
        (call-interceptors :after)
        ((fn [c] (pprint c) c))
        (get-in [:effects :dispatch]))))

(let [input example-event
      expected [:event/other :param1]]
  (t/deftest simple-handle
    (t/is (= expected (run example-event)))))
