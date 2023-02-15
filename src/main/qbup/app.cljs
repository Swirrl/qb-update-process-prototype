(ns qbup.app
  (:require [reagent.core :as r]
            [reagent.dom :as rd]
            ["md5" :as md5]
            [clojure.set :as set]
            [clojure.string :as str]))

(defmulti render-cell (fn [col-name _value]
                        col-name))

(defmethod render-cell :row-hash [_ value]
  (some-> value (subs 0 8)))

(defmethod render-cell :comp-hash [_ value]
  (some-> value (subs 0 8)))

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

(defn build-update-schema [user-schema]
  (-> user-schema
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

(def extended-schema (build-update-schema default-schema))

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
  {:mode :mode/view-cube
   :selected-revision (count example-history)

   :all-schemas [:default :update]

   :schemas {:default default-schema
             :update extended-schema}

   :selected-schema default-schema

   :history example-history

   :preview-data []
   })

(def init-state
  init-release)

(defonce state (r/atom init-state))

(defn change-screen! [mode]
  (swap! state assoc :mode mode))

(defn change-screen-button
  ([button-text mode]
   [change-screen-button {} button-text mode])
  ([opts button-text mode]
   [:button (assoc opts :on-click #(change-screen! mode))
    button-text]))

(defn select-revision! [rev]
  (swap! state assoc :selected-revision rev))

(defn revisions []
  (let [num-revisions (count (:history @state))
        revisions (range num-revisions 0 -1)
        fst-rev (first revisions)]
    [:select {:name "revision"
              :value (:selected-revision @state)
              :on-change #(select-revision! (parse-double (-> % .-target .-value)))}
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

(defn build-table-for-selected-revision [{:keys [selected-revision history]}]
  (sort-observations
   (nth (drop 1 (replay-changes history))
        (dec selected-revision))))

(defn filter-history-for-selected-revision [{:keys [selected-revision history]}]
  (->> history
       (take selected-revision)
       (map-indexed vector)
       (reverse)))

(defn latest-revision-selected? [{:keys [history selected-revision]}]
  (= selected-revision (count history)))

(defn schema-thead [selected-schema]
  (let [columns (:columns selected-schema)]
    [:thead
     [:tr
      (for [{:keys [name titles datatype]} columns]
        [:th {:key name} titles])]
     [:tr
      (for [{:keys [name titles datatype]} columns]
        [:td.datatype {:key name} [:em (str "(" (clojure.core/name datatype) ")")]])]]))

(defn table [{:keys [selected-schema selected-revision history] :as state}]
  [:table
   (let [columns (:columns selected-schema)]
     [:<>
      [schema-thead selected-schema]

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
        [:label {:for schema-name} (name schema-name)]]))
    " "
    (let [btn-opts (when-not (latest-revision-selected? @state)
                     {:disabled true})]
      [change-screen-button btn-opts
       "Create revision" :mode/create-revision])]])

(defn history [{:keys [history] :as state}]
  [:<>
   [:h3 "History"]
   [:ol.history-items
    (for [[revision change-set] (filter-history-for-selected-revision state)
          :let [revision (inc revision)]]
      ^{:key revision}
      [:li
       [:span.revision-number
        [:a {:href "#" :on-click #(select-revision! revision)}
         revision ":"]]
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

(defmulti transform-cell-datatype (fn [col val]
                                      (:datatype col)))

(defmethod transform-cell-datatype :double [col val]
    (parse-double val))

(defmethod transform-cell-datatype :default [col val]
    val)


(defn parse-csv [{:keys [columns]} csv-str]
  (let [cols (map :name columns)
        data (->> csv-str
                  (str/split-lines)
                  (map #(str/split % #","))
                  (map (fn [row]
                         (apply hash-map (interleave cols row)))))]

    (for [row data]
      (reduce (fn [row {col-name :name :as col}]
                (if (get row col-name)
                  (update row
                          col-name
                          #(transform-cell-datatype col %))
                  row))
              row
              (:columns extended-schema)))))

(defn write-csv [{:keys [history select-revision selected-schema include-header] :as state}]
  (let [table-data (build-table-for-selected-revision state)
        cols (map :name (:columns selected-schema))
        row-strs (str/join "\n"
                           (for [row table-data]
                             (str/join "," (map row cols))))]
    (if include-header
      (str (str/join "," (map name cols))
           row-strs)
      row-strs)))

(comment
  (write-csv @state))

(defn app []
  (let [st @state]
    (case (:mode st)
      :mode/view-cube [:<>
                       [:div.revision
                        [toggles]
                        [:div.inner-revision
                         [data-and-history st]]]]

      :mode/create-revision (let [update-schema (:update (:schemas st))]
                              [:<>
                               [:div.revision
                                [:div.inner-revision
                                 [:h3 (str "Create revision: " (inc (:selected-revision st)))]
                                 [:p "Please paste a UTF-8 CSV which conforms to the table schema below:"]
                                 [:table
                                  [schema-thead update-schema]
                                  [:tbody
                                   (for [row (:preview-data st)]
                                     ^{:key row}
                                     [:tr
                                      (for [coll-id (map :name (:columns update-schema))
                                            :let [cell (coll-id row)]]
                                        ^{:key coll-id} [:td
                                                         (render-cell coll-id cell)])])]]
                                 [:br]
                                 [:p "CSV:"]
                                 (let [update-preview! (fn [e]
                                                         (swap! state assoc
                                                               :preview-data (parse-csv (-> st :schemas :update) (-> e .-target .-value))))]
                                   [:textarea {:rows 20 :cols 145
                                               :default-value (write-csv (assoc st :selected-schema update-schema))
                                               :on-change update-preview!}])
                                 [:br]
                                 [change-screen-button "Cancel" :mode/view-cube]
                                 [:button {:on-click #(change-screen! :mode/view-cube)} "Submit"]]]])

      [:<> [:h1 "Error"]])))

(defn index-table [table]
  {:lookup-by-comp-hash (update-vals (group-by :comp-hash table) first)
   :lookup-by-row-hash (update-vals (group-by :row-hash table) first)})

(defn has-valid-dim-or-attribute-values?
  "This is a dummy check where we could/should run a validation against
  the set of values in the codelist.

  If the code does not exist in the list, then we should raise an error."
  [schema new-row]
  true)

(defn classify-row-change
  "Takes a default-schema an index-table as produced by `index-table`
  and a new-row, and classifies the changes that occurred in the row."
  [schema {:keys [lookup-by-comp-hash lookup-by-row-hash]}
   {:keys [row-hash comp-hash] :as new-row}]
  (let [old-row-hash row-hash
        new-row-hash (hash-row schema new-row)]

    (cond (= old-row-hash new-row-hash)
          [:unmodified old-row-hash]
          :else (let [old-comp-hash comp-hash
                      new-comp-hash (hash-components schema new-row)]
                  (if (= old-comp-hash new-comp-hash)
                    (let [measure-cols (->> (:columns schema)
                                            (filter (comp #{:qb/measure} :coltype))
                                            (map :name))
                          old-row (lookup-by-comp-hash old-comp-hash)
                          old-measures (map old-row measure-cols)
                          new-measures (map new-row measure-cols)]
                      [:measure-changed old-measures new-measures])
                    (if (has-valid-dim-or-attribute-values? schema new-row)

                      [:component-change new-row]
                      [:invalid-component-error :todo new-row] ;; TODO should validate values against codelists at which point this should be either an error or an append
                      )
                    )))))

(comment

  (def previous-table (build-table-for-selected-revision @state))

  (=
   '[:measure-changed (80.7) (78.3)]
   (classify-row-change default-schema
                        (index-table previous-table)
                        (assoc (first previous-table)
                               :life-expectancy 78.3)))

  (= [:component-change
      {:area "W06000022",
       :period "2004-01-01T00:00:00/P3Y",
       :sex "malez",
       :life-expectancy 77.1,
       :row-hash "392784af4099a8bea7ed89549a15ba88",
       :comp-hash "9f9aa997aeca139349bbf2b79998e879",
       :corrects "2151367e7ac2794836c4ffc4259fd3cc"}]

     (classify-row-change default-schema
                          previous-table
                          (assoc (second (build-table-for-selected-revision @state))
                                 :sex "malez")))

  (= [:unmodified "021b04a49352d9687daadd78e213d2ec"]
     (classify-row-change default-schema
                          previous-table
                          (first (build-table-for-selected-revision @state))))

  :foo)

(defn make-table-diff [schema orig-table new-table]
  (let [{:keys [lookup-by-comp-hash lookup-by-row-hash] :as lookups} (index-table orig-table)

        a-obs-hashes (set (keys lookup-by-row-hash))
        mods (map (partial classify-row-change schema lookups) new-table)]

    (cond
      (every? (comp #{:unmodified} first) mods) nil

      :else mods
      )

    ))

(comment

  (def init-table (build-table-for-selected-revision @state))

  (def table-from-csv (parse-csv extended-schema "021b04a49352d9687daadd78e213d2ec,W06000022,2004-01-01T00:00:00/P3Y,Female,80.7,1e5a82bd66c5e119f858bb1a1696041a,\n392784af4099a8bea7ed89549a15ba88,W06000022,2004-01-01T00:00:00/P3Y,Male,77.1,9f9aa997aeca139349bbf2b79998e879,2151367e7ac2794836c4ffc4259fd3cc\ne4f1a5dd7d76610a330dad0e5d40eb5e,W06000022,2005-01-01T00:00:00/P3Y,Female,80.9,3a049eff5351fa090f4946b4288343b9,\nf660ccad41c647ea059e70cdf6a56657,W06000022,2005-01-01T00:00:00/P3Y,Malez,72.9,844247dc32efa49f35582ee66ad06442,"))

  (make-table-diff default-schema
                   init-table
                   table-from-csv)

;; => {:area "W06000022",
;;     :period "2004-01-01T00:00:00/P3Y",
;;     :sex "Female",
;;     :life-expectancy 80.7,
;;     :row-hash "021b04a49352d9687daadd78e213d2ec",
;;     :comp-hash "1e5a82bd66c5e119f858bb1a1696041a"}

  (first (parse-csv extended-schema "021b04a49352d9687daadd78e213d2ec,W06000022,2004-01-01T00:00:00/P3Y,Female,80.7,1e5a82bd66c5e119f858bb1a1696041a,\n392784af4099a8bea7ed89549a15ba88,W06000022,2004-01-01T00:00:00/P3Y,Male,77.1,9f9aa997aeca139349bbf2b79998e879,2151367e7ac2794836c4ffc4259fd3cc\ne4f1a5dd7d76610a330dad0e5d40eb5e,W06000022,2005-01-01T00:00:00/P3Y,Female,80.9,3a049eff5351fa090f4946b4288343b9,\nf660ccad41c647ea059e70cdf6a56657,W06000022,2005-01-01T00:00:00/P3Y,Male,72.9,844247dc32efa49f35582ee66ad06442,"))

;; => {:sex "Female",
;;     :life-expectancy "80.7",
;;     :area "W06000022",
;;     :period "2004-01-01T00:00:00/P3Y",
;;     :comp-hash "1e5a82bd66c5e119f858bb1a1696041a",
;;     :row-hash "021b04a49352d9687daadd78e213d2ec"}

  :foo)



(defn ^:dev/before-load stop []
  (js/console.log "stopping"))

(defn ^:export ^:dev/after-load init []
  (rd/render [app] (js/document.getElementById "app")))
