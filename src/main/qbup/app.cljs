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

(defmethod render-cell :operation-type [_ value]
  (str/upper-case (name value)))

(defmethod render-cell :default [_ value]
  (str value))

(def default-schema
  {:schema-name :schema/default
   :columns [{:name :area
              :titles "Area"
              :datatype :string
              :coltype :qb/dimension
              :valid-value? (fn [v] (= 9 (count v)))}
             {:name :period
              :titles "Period"
              :datatype :period
              :coltype :qb/dimension
              :valid-value? (constantly true)}
             {:name :sex
              :titles "Sex"
              :datatype :string
              :coltype :qb/dimension
              :valid-value? #{"Male" "Female" "Other"}}
             {:name :life-expectancy
              :titles "Life Expectancy"
              :datatype :double
              :coltype :qb/measure
              :valid-value? double?}]})

(defn build-update-schema [user-schema]
  (-> user-schema
      (assoc :schema-name :schema/extended)
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
       (filter (comp #{:qb/dimension :qb/attribute} :coltype)) ;; TODO should probably have a dim-hash too
       (map :name)))

(defn build-duplicate-schema [schema]
  (-> schema
      (assoc :schema-name :schema/duplicate)
      (update-in [:columns]
                 (partial cons
                          {:name :row-hash
                           :titles "ID"
                           :datatype :uri}))
      (update-in [:columns] concat
                 [{:name :duplicate-count
                   :titles "Duplicate Count"
                   :datatype :double}])))

(defn build-corrections-schema [schema]
  (-> schema
      (assoc :schema-name :schema/corrections)
      (update-in [:columns]
                 (partial cons
                          {:name :row-hash
                           :titles "ID"
                           :datatype :uri}))
      (update-in [:columns] concat
                 [{:name :previous-value
                   :titles "Previous Value"
                   :datatype :double}
                  {:name :corrects
                   :titles "Corrects Observation"
                   :datatype :uri}])))

(defn build-delta-schema [schema]
  (-> schema
      (assoc :schema-name :schema/delta)
      (update-in [:columns]
                 (partial cons
                          {:name :row-hash
                           :titles "Observation ID"
                           :datatype :uri}))
      (update-in [:columns]
                 (partial cons
                          {:name :operation-type
                           :titles "Operation"
                           :datatype :string}))
      (update-in [:columns]
                 (partial cons
                          {:name :revision-id
                           :titles "Revision ID"
                           :datatype :double}))
      (update-in [:columns] concat
                 [{:name :corrects
                   :titles "Corrects Observation"
                   :datatype :uri}
                  {:name :comment
                   :titles "Commit message"
                   :datatype :string}])))

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

                        {:append (make-rows {:area "W06000022" :period "2999-01-01T00:00:00/P3Y" :sex "Male" :life-expectancy 77.1})
                         :comment "Add more data with a mistake we will remove in a future revision"}

                        {:delete (make-rows {:area "W06000022" :period "2999-01-01T00:00:00/P3Y" :sex "Male" :life-expectancy 77.1})
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

   :all-schemas [:schema/default :schema/extended]

   :schemas {:schema/default default-schema
             :schema/extended extended-schema}

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

(defn build-table-for-last-revision [{:keys [history]}]
  (build-table-for-selected-revision {:selected-revision (count history)
                                      :history history}))

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

(defn history [{:keys [_history] :as state}]
  [:<>
   [:h3 "History"]
   [:a {:href "#"
        :on-click (fn [_] (change-screen! :mode/view-deltas))} "View all"]
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

(defmulti transform-cell-datatype (fn [col _val]
                                    (:datatype col)))

(defmethod transform-cell-datatype :double [_col val]
  (parse-double val))

(defmethod transform-cell-datatype :default [_colcol val]
  val)


(defn parse-csv [{:keys [columns]} csv-str]
  (when-not (str/blank? (str/trim csv-str))
    (let [cols (map :name columns)
          data (->> csv-str
                    (str/split-lines)
                    (map #(str/split % #",")))
          cell-counts (distinct (map count data))]
      (if (not= 1 (count cell-counts))
        (throw (ex-info "Schema error" {:schema-error :ragged-rows}))

        (if (not= (count columns)
                  (first cell-counts))
          (throw (ex-info "Schema error" {:schema-error :invalid-tidy-data}))
          (let [schemad-data (->> data
                                  (map (fn [row]
                                         (apply hash-map (interleave cols row)))))]
            (for [row schemad-data]
              (reduce (fn [row {col-name :name :as col}]
                        (if (get row col-name)
                          (update row
                                  col-name
                                  #(transform-cell-datatype col %))
                          row))
                      row
                      columns))))))))

(comment
  (parse-csv default-schema "W06000022,2004-01-01T00:00:00/P3Y,Female\n")

  (parse-csv default-schema "W06000022,2004-01-01T00:00:00/P3Y,Female,80.7\nW06000022,2004-01-01T00:00:00/P3Y,Male,77.1\nW06000022,2005-01-01T00:00:00/P3Y,Female,80.9\nW06000022,2005-01-01T00:00:00/P3Y,Malez,72.9,213"))

(defn write-csv [{:keys [selected-schema include-header] :as state}]
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

(defn tab-control [{:keys [id label] :as tab-opts}]
  [:<> [:input (assoc (dissoc tab-opts :label)
                      :type :radio)]
   [:label {:for id} label]])

(defn tabs [{:keys [_name default-tab _tabs]} _tab-panes]
  (let [selected-tab (r/atom default-tab)]
    (fn [{:keys [name _default-tab tabs]} tab-panes]
      (let [tab-controls (vec
                          (cons :span.tab-controls
                                (mapv (fn [tab-id]
                                        (let [{:keys [label disabled] :as tab-opts} (get tab-panes tab-id)]
                                          [tab-control (cond-> {:id tab-id
                                                                :name name
                                                                :label label
                                                                :value tab-id
                                                                :checked (= tab-id @selected-tab)
                                                                :on-change #(reset! selected-tab tab-id)}
                                                         disabled (assoc :disabled true))]))
                                      tabs)))
            selected-tab-pane (:pane (get tab-panes @selected-tab))]
        [:div.tabs
         tab-controls
         [:div.tab-pane {}
          selected-tab-pane]]))))

;;;;;;;;;; validations / changes

(defn index-table [table]
  {:lookup-by-comp-hash (update-vals (group-by :comp-hash table) first)
   :lookup-by-row-hash (update-vals (group-by :row-hash table) first)})

(defn has-valid-dim-or-attribute-values?
  "This is a dummy check where we could/should run a validation against
  the set of values in the codelist.

  If the code does not exist in the list, then we should raise an error."
  [_schema _new-row]
  true)

(defn apply-validations
  "Appends an :errors key to rows with validation failures."
  [{:keys [columns] :as _schema} table]
  (let [validated-rows (for [row table]
                         (reduce (fn [row {:keys [name valid-value?] :as _col}]
                                   (if (boolean (valid-value? (get row name)))
                                     row
                                     (merge-with set/union row {:errors #{name}})))
                                 row columns))]
    (if (some :errors validated-rows)
      {:errors (filter :errors validated-rows)}
      {})))

(defn make-table-diff [{:keys [columns] :as schema} orig-table new-table]
  (let [{:keys [lookup-by-comp-hash lookup-by-row-hash]} (index-table orig-table)
        new-table-hashed (hash-rows schema new-table)
        in-orig-table? (set (keys lookup-by-comp-hash))
        appended-data (filter (comp (complement in-orig-table?) :comp-hash)
                              new-table-hashed)]
    (-> (apply-validations schema new-table)
        (assoc
         :duplicate (reduce-kv
                     (fn [acc k-row v]
                       (let [n (count v)]
                         (if (>= n 2)
                           (conj acc (assoc k-row :duplicate-count n
                                            :errors #{:duplicate-count}))
                           acc)))
                     #{}
                     (group-by identity new-table-hashed))
         :append appended-data

         :delete (let [in-new-table? (set (map :comp-hash new-table-hashed))]
                   (filter (fn [old-row]
                             (let [h (:comp-hash old-row)]
                               (and (in-orig-table? h)
                                    (not (in-new-table? h)))))
                           orig-table))
         :corrections (->> new-table-hashed
                           (map (fn [new-row]
                                  (let [orig-row (lookup-by-comp-hash (:comp-hash new-row))
                                        measure-col (:name (first (filter (comp #{:qb/measure} :coltype) columns)))]
                                    (if (and orig-row
                                             (not (lookup-by-row-hash (:row-hash new-row))))
                                      (assoc new-row
                                             :corrects (:row-hash orig-row)
                                             :previous-value (get orig-row measure-col)
                                             :previous-obs orig-row)
                                      new-row))))
                           (filter :corrects))))))



(comment

  (def init-table (build-table-for-selected-revision @state))

  (def table-from-csv (parse-csv default-schema "W06000022,2004-01-01T00:00:00/P3Y,Female,80.7\nW06000022,2004-01-01T00:00:00/P3Y,Male,77.1\nW06000022,2005-01-01T00:00:00/P3Y,Female,80.9\nW06000022,2005-01-01T00:00:00/P3Y,Malez,72.9"))

  ;; errors
  (make-table-diff default-schema
                   init-table
                   table-from-csv)

  (def appended-table (concat init-table
                              [{:area "W06000023",
                                :period "2004-01-01T00:00:00/P3Y",
                                :sex "Female",
                                :life-expectancy 90.0}
                               {:area "W06000023",
                                :period "2004-01-01T00:00:00/P3Y",
                                :sex "Female",
                                :life-expectancy 90.0}]))

  (filter (comp (complement (set (keys (:lookup-by-comp-hash (index-table init-table)))))
                :comp-hash)
          (hash-rows default-schema appended-table))

  (make-table-diff default-schema
                   init-table
                   appended-table)

  :foo)

(defn schemad-data-table [schema table-data]
  [:table
   [schema-thead schema]

   [:tbody
    (for [[row-num {:keys [errors] :as row}] (take 10 (map-indexed vector table-data))]
      ^{:key row-num}
      [:tr
       (for [coll-id (map :name (:columns schema))
             :let [cell (coll-id row)]]
         ^{:key coll-id} [:td (when (get errors coll-id)
                                {:style {:background-color "pink"}})
                          (render-cell coll-id cell)])])]])

(defn history-data-table [_schema _table-data]
  (let [hover-value (r/atom nil)]
    (fn [schema table-data]
      [:<>
       (when-not (nil? @hover-value)
         [:<>
          [:button {:on-click #(reset! hover-value nil)}
           "Clear filter"]
          [:br]
          (str "Filtered to: " @hover-value " ")])
       [:table
        [schema-thead schema]

        [:tbody
         (for [[row-num {:keys [operation-type] :as row}] (->> (if (nil? @hover-value)
                                                                 table-data
                                                                 (filter (fn [row]
                                                                           (or (= @hover-value (get row :row-hash))
                                                                               (= @hover-value (get row :corrects))))
                                                                         table-data))
                                                               (map-indexed vector))]
           ^{:key row-num}
           [:tr (case operation-type
                  :append {:style {:background-color "#9be9a8"}} ;; green
                  :delete {:style {:background-color "pink"}})
            (for [coll-id (map :name (:columns schema))
                  :let [cell (coll-id row)]]
              ^{:key coll-id} [:td (when (#{:row-hash :corrects} coll-id)
                                     {:style {:cursor "pointer"}
                                      :on-click (fn [_]
                                                  (reset! hover-value cell))})
                               (render-cell coll-id cell)])])]]])))

(defn failures? [{:keys [errors duplicate] :as change-data}]
  (boolean (or (seq errors)
               (seq duplicate))))

(defn error-display [schema {:keys [errors duplicate]}]
  (let [num-errors (count errors)]
    [:<>
     [:h2 (if (or (seq errors))
            "❌"
            "✅") " Validations"]

     [:h3 (if (seq errors)
            "❌"
            "✅") " Observation validations"]
     (if (seq errors)
       [:<>
        "The following " (if (>= num-errors 2)
                           (str num-errors " observations do not conform to the table schema:")
                           " observation does not conform to the table schema:")
        [schemad-data-table schema errors]
        (when (< 10 num-errors)
          [:em "... and " (- num-errors 10) " more "])]

       [:<>
        [:p "Congratulations all observations are valid and conform to the table schema."]
        #_[:table
           [schema-thead schema]]])

     [:h3 (if (seq duplicate)
            [:<>
             (str "❌ There are duplicated observations")]
            (str "✅" " No duplicate observations found"))]
     (when (seq duplicate)
       [schemad-data-table (build-duplicate-schema schema) duplicate])]))

(defn pane [opts & more]
  [:div.inner-revision {:style {:border "1px solid #999"}}
   (vec (concat [:<>] more))])

(defn appends-pane [schema appends-data]
  [pane
   [:h3 "Appends"]
   [:p "The following appends were detected"]
   [schemad-data-table schema appends-data]])

(defn deletes-pane [schema deletes-data]
  [pane
   [:h3 "Deletes"]
   [:p "The following deletions were detected"]
   [schemad-data-table schema deletes-data]])

(defn corrections-pane [schema corrections-data]
  [pane
   [:h3 "Corrections"]
   [:p "The following corrections were detected"]
   [schemad-data-table (build-corrections-schema schema) corrections-data]])

(defn categorise-change-tab [{:keys [errors duplicate append delete corrections]}]
  (or (and (seq append) :tab/appends)
      (and (seq delete) :tab/deletes)
      (and (seq corrections) :tab/corrections)
      :no-changes))

(defn change-set-tabs [{:keys [default-tab name] :as opts}
                       schema {:keys [errors duplicate append delete corrections] :as preview-changes}]
  [tabs {:default-tab default-tab
         :name name
         :tabs [:tab/appends :tab/deletes :tab/corrections]}
   {:tab/appends {:label [:<> "Appends (" (count append) ")"]
                  :pane [appends-pane schema append]
                  :disabled (zero? (count append))}
    :tab/deletes {:label [:<> "Deletes (" (count delete) ")"]
                  :pane [deletes-pane schema delete]
                  :disabled (zero? (count delete))}
    :tab/corrections {:label [:<> "Corrections (" (count corrections) ")"]
                      :pane [corrections-pane schema corrections]
                      :disabled (zero? (count corrections))}}])

(defn change-set [schema {:keys [errors duplicate append delete corrections] :as preview-changes}]
  [:<>
   [:h3 "Change Set"]
   (let [default-tab (categorise-change-tab preview-changes)]
     [:<>
      (if (= default-tab :no-changes)
        [:p "There are no changes detected in your data."]
        [:p "The following changes have been detected in the provided data:"])

      (when-not (= default-tab :no-changes)
        [:<>
         [change-set-tabs {:default-tab default-tab
                           :name :change-type} schema preview-changes]])])])

(defn modal [{:keys [cancel-view]} & more]
  [:<>
   [:div.revision
    [:div.inner-revision
     [change-screen-button "Cancel" cancel-view]
     (vec (concat [:<>] more))]]])

;; NOTE this is a bit hacky we could represent corrections as
;; append/deletes before this
(defn rewrite-corrections
  "Rewrite corrections in change-set preview data as append/delete
  operations for history."
  [{:keys [corrections] :as change-set}]

  (let [correction-appends (set (map #(dissoc % :previous-obs :previous-value) corrections))
        correction-deletes (set (map :previous-obs corrections))]

    (-> change-set
        (update :append set/union correction-appends)
        (update :delete set/union correction-deletes)
        (dissoc :corrections))))

(defn save-commit [{:keys [history] :as state} change-set message]
  (let [commit (-> change-set
                   (update :append set)
                   (update :delete set)
                   ;;(update :corrections set) ;; TODO corrections
                   (assoc :comment message)
                   (dissoc :duplicate)
                   (rewrite-corrections))]
    (-> state
        (update :history conj commit)
        (assoc :selected-revision (inc (count history)))
        (dissoc :preview-data)
        (assoc :mode :mode/view-cube))))

(defn app []
  (let [{:keys [history] :as st} @state]
    (case (:mode st)
      :mode/view-cube [:<>
                       [:div.revision
                        [toggles]
                        [:div.inner-revision
                         [data-and-history st]]]]

      :mode/view-deltas [:<>
                         [modal {:cancel-view :mode/view-cube}
                          [:h1 "View History"]
                          (let [{:keys [history]} st
                                schema (:schema/default (:schemas st))]

                            [history-data-table
                             (build-delta-schema schema)
                             (->> history
                                  (map-indexed vector)
                                  (reverse)
                                  (mapcat (fn [[revision-num {:keys [append delete comment] :as history-item}]]
                                            ^{:key hi}
                                            (map (fn [row]
                                                   (assoc row
                                                          :revision-id (inc revision-num)
                                                          :comment comment))
                                                 (concat (map #(assoc % :operation-type :append)
                                                              append)
                                                         (map #(assoc % :operation-type :delete)
                                                              delete))))))])]]

      :mode/create-revision (let [schema (:schema/default (:schemas st))]
                              [modal {:cancel-view :mode/view-cube}
                               (let [next-revision (inc (:selected-revision st))]
                                 [:<>
                                  [:h1 (str "Create revision: " next-revision)]]
                                 (let [{:keys [preview-changes]} st]
                                   [:<>
                                    (let [schema-error (:schema-error st)]

                                      [:<>

                                       (cond
                                         schema-error [:<>
                                                       [:h2 "❌ Invalid Schema"]
                                                       [:p (cond
                                                             (= :invalid-tidy-data schema-error)
                                                             "Supplied tidy data does not match expected schema"
                                                             (= :ragged-rows schema-error)
                                                             "Supplied data is not tidy (it has ragged rows) and does not match the expected schema")]]
                                         (failures? preview-changes) [error-display schema (select-keys preview-changes [:errors :duplicate])]
                                         :else [change-set schema preview-changes])])
                                    (when (and (not (failures? preview-changes))
                                               (not= :no-changes (categorise-change-tab preview-changes)))
                                      [:<>
                                       [:br]
                                       [:button {:on-click (fn [_]
                                                             (swap! state assoc :candidate/changes preview-changes)
                                                             (change-screen! :mode/make-change-set))} "Create Change Set"]])

                                    [:p "Please provide a CSV which conforms to the releases table schema."]
                                    [:em "(Note we have loaded the data from the previous revision r" (count history) ")"]
                                    (let [update-preview! (fn [e]
                                                            (try (let [preview-data (parse-csv schema (-> e .-target .-value))]

                                                                   (swap! state assoc
                                                                          :preview-data preview-data
                                                                          :preview-changes (make-table-diff schema (build-table-for-last-revision st) preview-data))
                                                                   (swap! state dissoc :schema-error))
                                                                 (catch js/Error ex
                                                                   (let [{:keys [schema-error]} (ex-data ex)]
                                                                     (swap! state assoc
                                                                            :schema-error schema-error)))))]

                                      [:textarea {:rows 20 :cols 145
                                                  :default-value (write-csv (assoc st :selected-schema schema))
                                                  :on-change update-preview!}])]))])

      :mode/make-change-set [modal {:cancel-view :mode/view-cube}
                             (let [next-revision (inc (:selected-revision st))]
                               [:<>
                                [:h1 (str "Create revision: " next-revision)]])
                             (let [schema (:schema/default (:schemas st))
                                   {:keys [candidate/changes]} st]

                               [:<>
                                [:p "Commit message:"]
                                [:textarea {:rows 5 :cols 80
                                            :on-change (fn [e]
                                                         (swap! state assoc :commit/message (-> e .-target .-value)))}]
                                [:br]
                                [:button {:on-click (fn [_]
                                                      (swap! state save-commit changes (:commit/message @state)))}
                                 "Commit change set"]
                                [change-set schema changes]])]

      [:<> [:h1 "Error"]])))



(defn ^:dev/before-load stop []
  (js/console.log "stopping"))

(defn ^:export ^:dev/after-load init []
  (rd/render [app] (js/document.getElementById "app")))
