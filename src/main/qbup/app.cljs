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

(defmethod render-cell :corrects [_ value]
  (some-> value (subs 0 8)))

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
       (filter (comp #{:qb/dimension :qb/attribute} :coltype))
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

(defn simulate-user-correction
  "Simulate what a correction by a user might look like, i.e. it will be
  an old-row where the component hashes no longer match the data."
  [schema row column-id new-value]
  (let [original (first (hash-rows schema [row]))]
    (assoc original column-id new-value)))

(defn make-correction [old-row new-kvps]
  (let [old-obs (first (make-rows old-row))
        new-obs (-> (first (make-rows (merge old-row new-kvps)))
                    (assoc :corrects (:row-hash old-obs)))]
    {:append #{new-obs}
     :delete #{old-obs}}))

(def example-history (let [obs-to-correct {:area "W06000022" :period "2004-01-01T00:00:00/P3Y" :sex "Male" :life-expectancy 76999.7}]
                       [{:append (make-rows obs-to-correct
                                            {:area "W06000022" :period "2004-01-01T00:00:00/P3Y" :sex "Female" :life-expectancy 80.7})
                         :comment "Add initial 2004 data"}

                        {:append
                         (make-rows {:area "W06000022" :period "2005-01-01T00:00:00/P3Y" :sex "Male" :life-expectancy 72.9}
                                    {:area "W06000022" :period "2005-01-01T00:00:00/P3Y" :sex "Female" :life-expectancy 80.9})
                         :comment "Upload 2005 data"}

                        {:append (make-rows {:area "mistake" :period "2005-01-01T00:00:00/P3Y" :sex "Male" :life-expectancy 77.1})
                         :comment "Add more data"}

                        {:delete (make-rows {:area "mistake" :period "2005-01-01T00:00:00/P3Y" :sex "Male" :life-expectancy 77.1})
                         :comment "Delete mistaken observation entry"}

                        (-> (make-correction obs-to-correct ;; correct phat fingered obs
                                             {:life-expectancy 77.1})
                            (assoc :comment "Make a correction where the cat ran over the keyboard"))]))



(defn replay-changes
  "Returns a lazy sequence of reductions showing the state of the
  table at every revision"
  [history]
  (reductions
   (fn [current-table {:keys [append delete]}]
     (cond-> current-table
       append (set/union append)
       delete (set/difference delete)))
   #{} history))



(def init-release
  {:selected-revision (count example-history)

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
              :on-change #(swap! state assoc :selected-revision (parse-double (-> % .-target .-value)))}
     (doall
      (for [r revisions]
        [:option {:value r
                  :key r}
         (if (= fst-rev r)
           (str r " (latest)")
           r)]))]))

(defn sort-observations [row-set]
  (sort-by (apply juxt (component-schema default-schema))
           row-set))

(defn build-table-for-selected-revision [{:keys [selected-revision history] :as state}]
  (sort-observations
   (nth (drop 1 (replay-changes history))
        (dec selected-revision))))

(defn filter-history-for-selected-revision [{:keys [selected-revision history] :as state}]
  (->> history
       (take selected-revision)
       (map-indexed vector)
       (reverse)))

(defn table [{:keys [selected-schema selected-revision history] :as state}]
  [:table
   (let [columns (:columns selected-schema)]
     [:<>
      [:thead
       [:tr
        (for [{:keys [name titles datatype]} columns]
          [:th {:key name} titles])]
       [:tr
        (for [{:keys [name titles datatype]} columns]
          [:td.datatype {:key name} [:em (str "(" (clojure.core/name datatype) ")")]])]]

      [:tbody
       (let [current-table (build-table-for-selected-revision state)]
         (for [row current-table]
           ^{:key row}
           [:tr
            (for [coll-id (map :name columns)
                  :let [cell (coll-id row)]]
              ^{:key coll-id} [:td
                               (render-cell coll-id cell)])]))]])])

(defn toggles []
  [:div.toggles
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
        [:label {:for schema-name} (name schema-name)]]))]])

(defn history [{:keys [history] :as state}]
  [:<>
   [:h3 "History"]
   [:ol.history-items
    (for [[revision change-set] (filter-history-for-selected-revision state)
          :let [revision (inc revision)]]
      ^{:key revision}
      [:li
       [:span.revision-number
        revision ":"]
       [:span.history-comment (:comment change-set) " "]
       [:span
        (let [corrections (count (filter :corrects (:append change-set)))]
          [:em
           "("
           [:span.change-count
            (- (count (:append change-set))
               corrections) " appends "]
           [:span.change-count
            (- (count (:delete change-set))
               corrections) " deletes and "]
           [:span.change-count
            corrections " corrections"]
           ")"])]])]])

(defn download []
  #_[:a {:href "#"} "download"])

(defn data-and-history [state]
  [:<>
   [table state]
   [download]
   [history state]])

(defn app []
  [:<>
   [:h1 "Cube Update Process Prototype"]
   [:div.revision
    [toggles]
    [:div.inner-revision
     (let [state @state]
       [data-and-history state])]]
   ])



(defn ^:dev/before-load stop []
  (js/console.log "stopping"))

(defn ^:export ^:dev/after-load init []
  (rd/render [app] (js/document.getElementById "app")))
