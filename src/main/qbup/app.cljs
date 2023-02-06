(ns qbup.app
  (:require [reagent.core :as r]
            [reagent.dom :as rd]
            ["md5" :as md5]
            [clojure.set :as set]))

(defmulti render-cell (fn [col-name _value]
                        col-name))

(defmethod render-cell :row-hash [_ value]
  (subs value 0 8))

(defmethod render-cell :comp-hash [_ value]
  (subs value 0 8))

(defmethod render-cell :default [_ value]
  (str value))

(def default-schema
  {:schema-name :default
   :columns [{:name :area
              :titles "Area"
              :datatype :string
              :coltype :qb/dimension}
             {:name :period
              :titles "Period"
              :datatype :period
              :coltype :qb/dimension}
             {:name :sex
              :titles "Sex"
              :datatype :string
              :coltype :qb/dimension}
             {:name :life-expectancy
              :titles "Life Expectancy"
              :datatype :double
              :coltype :qb/measure}]})

(def extended-schema (-> default-schema
                         (assoc :schema-name :update)
                         (update-in [:columns]
                                    (partial cons
                                             {:name :row-hash
                                              :titles "ID"
                                              :datatype :uri}))
                         (update-in [:columns] concat
                                    [{:name :comp-hash
                                      :titles "Component Hash"
                                      :datatype :md5}
                                     {:name :corrects
                                      :titles "Corrects"
                                      :datatype :uri}])))

(defn hash-row [{:keys [columns] :as schema} row]
  (md5 (map row (map :name columns))))

(defn component-schema [{:keys [columns] :as schema}]
  (->> columns
       (filter (comp #{:qb/component :qb/dimension} :coltype))
       (map :name)))

(defn hash-components [schema row]
  (let [comp-cols (component-schema schema)]
    (md5 (map row comp-cols))))

(defn hash-rows [schema table-data]
  (map (fn [row]
         (assoc row
                :row-hash (hash-row schema row)
                :comp-hash (hash-components schema row)))
       table-data))

(defn make-rows [& row-data]
  (set (hash-rows default-schema row-data)))

(def example-history [{:append (make-rows {:area "W06000022" :period "2004-01-01T00:00:00/P3Y" :sex "Male" :life-expectancy 76.7}
                                          {:area "W06000022" :period "2004-01-01T00:00:00/P3Y" :sex "Female" :life-expectancy 80.7})
                       :comment "Add initial data"}

                      {:append
                       (make-rows {:area "W06000022" :period "2005-01-01T00:00:00/P3Y" :sex "Male" :life-expectancy 77.1}
                                  {:area "W06000022" :period "2005-01-01T00:00:00/P3Y" :sex "Female" :life-expectancy 80.9})
                       :comment "Add 2005 data"}

                      {:append (make-rows {:area "mistake" :period "2005-01-01T00:00:00/P3Y" :sex "Male" :life-expectancy 77.1})
                       :comment "mistake accidentally added"}

                      {:delete (make-rows {:area "mistake" :period "2005-01-01T00:00:00/P3Y" :sex "Male" :life-expectancy 77.1})
                       :comment "Delete mistaken observation entry"}])

(defn replay-changes
  "Returns a lazy sequence of reductions showing the state of the
  table at every revision"
  [history]
  (reductions
   (fn [current-table {:keys [append delete amend]}]
     (cond
       append (set/union current-table append)
       delete (set/difference current-table delete)
       amend current-table ;; TODO
       ))
   #{} history))



(def init-release
  {:selected-revision 4

   :all-schemas [:default :update]

   :schemas {:default default-schema
             :update extended-schema}

   :selected-schema default-schema

   :history example-history

   })

(def init-state
  init-release)

(def state (r/atom init-state))

(defn revisions []
  (let [num-revisions (count (:history @state))
        revisions (range num-revisions 0 -1)
        fst-rev (first revisions)]
    [:select {:name "revision"
              :default-value (:selected-revision @state)
              :on-change #(swap! state assoc :selected-revision (-> % .-target .-value))}
     (doall
      (for [r revisions]
        [:option {:value r
                  :key r}
         (if (= fst-rev r)
           (str r " (latest)")
           r)]))]))

(comment

  (hash-rows
            default-schema
            #{{:area "W06000022" :period "2004-01-01T00:00:00/P3Y" :sex "Male" :life-expectancy 76.7}
              {:area "W06000022" :period "2004-01-01T00:00:00/P3Y" :sex "Female" :life-expectancy 80.7}
              {:area "W06000022" :period "2005-01-01T00:00:00/P3Y" :sex "Male" :life-expectancy 77.1}
              {:area "W06000022" :period "2005-01-01T00:00:00/P3Y" :sex "Female" :life-expectancy 80.9}
              {:area "W06000022" :period "2006-01-01T00:00:00/P3Y" :sex "Male" :life-expectancy 77.0}})
  )

(defn sort-observations [row-set]
  (sort-by (apply juxt (component-schema default-schema))
           row-set))

(defn table []
  [:table
   (let [columns (:columns (:selected-schema @state))]
     [:<>
      [:thead
       [:tr
        (for [{:keys [name titles datatype]} columns]
          [:th {:key name} titles])]
       [:tr
        (for [{:keys [name titles datatype]} columns]
          [:td.datatype {:key name} [:em (str "(" (clojure.core/name datatype) ")")]])]]

      [:tbody
       (let [{:keys [history selected-revision] :as state} @state
             current-table (nth (replay-changes history)
                                (dec selected-revision))]
         (for [row current-table]
           ^{:key row}
           [:tr
            (for [coll-id (map :name columns)
                  :let [cell (coll-id row)]]
              ^{:key coll-id} [:td
                               (render-cell coll-id cell)])]))]])])

(defn toggles []
  [:fieldset
   "revision: " [revisions]
   " schema: "
   (doall
    (for [schema-name (:all-schemas @state)]
      [:<> {:key schema-name}
       [:input {:type :radio
                :name "selected-schema"
                :value schema-name
                :checked (boolean (= schema-name (:schema-name (:selected-schema @state))))
                :on-change #(swap! state assoc :selected-schema (schema-name (:schemas @state)))}]
       [:label {:for schema-name} (name schema-name)]]))])

(defn app []
  [:<>
   [:h1 "Cube Update Process Prototype"]
   [toggles]
   [table]])



(defn ^:dev/before-load stop []
  (js/console.log "stopping"))

(defn ^:export ^:dev/after-load init []
  (rd/render [app] (js/document.getElementById "app")))
