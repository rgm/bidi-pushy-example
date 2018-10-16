(ns sample.core
  "trying to keep one source of truth and have a search object
   derive the location bar, an input text and a search result.
   should change when:
   - we click a link in a started app
   - we change the input directly
   - we come in via a link directly"
  (:require [bidi.bidi :as bidi]
            [cemerick.url]
            [pushy.core :as pushy]
            [re-frame.core :as rf]
            [reagent.core :as rg]
            [clojure.walk :refer [keywordize-keys]]
            [semantic-ui-react :as sui]
            [stylefy.core :as stylefy]
            [taoensso.timbre :refer-macros [debug warn spy]])
  (:import [goog Uri]
           [goog.async Throttle]))

;; - route via a-href
;; - route via dispatch in form
;; - route restoration on reload
;; - set form value on route change
;; - set URL value on form change
;; - debounce pushy set-token!

(enable-console-print!)

(def throttle-interval 150)

(def app-routes
  ["/" [["" :home]
        ["catalogue/" {"" :details/list
                       [:id "/"] :details/view-one}]
        [true :error/not-found]]])

(defn match-route
  "wrap bidi match-route* to make use of query params"
  [routes path & {:as options}]
  (let [route (bidi/match-route* routes path options)
        query (some-> path Uri. .getQuery cemerick.url/query->map keywordize-keys)]
    (spy :info (assoc route :query-params query))))

(def history
  (pushy/pushy (fn [match]
                 (rf/dispatch [:set-route match]))
               (partial match-route app-routes)))

(def start-router! #(pushy/start! history))

(defn update-values
  [f m]
  (reduce (fn [acc [k v]]
            (assoc acc k (f v)))
          {} m))

(defn make-new-token
  [loc m]
  (let [loc (bidi/path-for app-routes :details/list)]
    (str loc
         "?"
         (cemerick.url/map->query (update-values cemerick.url/url-encode m)))))

(def update-url-bar
  (letfn [(update-url-bar*
            [loc m]
            (pushy/set-token!
              history (make-new-token loc m)))]
    (Throttle. update-url-bar* throttle-interval)))

(defn filter-things [search things]
  (if (empty? (:q search))
    things
    (let [re (re-pattern (:q search))]
      (filter #(re-find re %) things))))

;;; re-frame events, subs

(def default-db
  {:route [:home]
   :things ["hi" "whatup"
            "things are good" "some test data"]})

(rf/reg-cofx
  :window-location
  (fn [cofx _]
    (assoc cofx :window-location
           (-> js/window .-location .-href))))

(rf/reg-fx
  :encode-search-into-url
  (fn [[current-location search]]
    (.fire update-url-bar current-location search)))

(rf/reg-fx
  :push-state
  (fn [route]
    (prn route)
    (let [path (spy :warn (bidi/path-for app-routes route))]
      #_(pushy/set-token! history path))))

(rf/reg-event-db
 :initialize-db
 (fn [_ _]
   default-db))

(defn extract-search
  [url]
  (->> url
       cemerick.url/url
       :query
       keywordize-keys))

(rf/reg-event-fx
  :rehydrate-from-url
  [(rf/inject-cofx :window-location)]
  (fn [{:keys [window-location db]} _]
    (let [search (extract-search window-location)]
      {:db (assoc db :search search)})))

(rf/reg-event-db
  :set-route
  (fn [db [_ route]]
    (assoc db :route route)))

;;; let pushstate dispatch take over if we change via an event
(rf/reg-event-fx
  :set-route-programatically
  (fn [_ [_ route]]
    {:push-state route}))

(rf/reg-event-fx
  :set-search-q
  [(rf/inject-cofx :window-location)]
  (fn [{:keys [window-location db]} [_ search-text]]
    (let [encoded-search (assoc (:search db) :q search-text)]
    {:db (assoc db :search encoded-search)
     :encode-search-into-url [window-location encoded-search]})))

(rf/reg-event-fx
  :clear-search
  (fn [{:keys [db]} _]
    {:db (dissoc db :search)}))

(rf/reg-sub
  :router/current-route
  (fn [db _]
    (get-in db [:route :handler])))

(rf/reg-sub
  :search
  (fn [db _]
    (:search db)))

(rf/reg-sub
  :things
  (fn [db _]
    (:things db)))

(rf/reg-sub
  :filtered-things
  (fn [_ _]
    [(rf/subscribe [:things])
     (rf/subscribe [:search])])
  (fn [[things search] _]
    (filter-things search things)))

;;; reagent components

(defn Home
  []
  [:> sui/Header "Home route"])

(defn DetailList
  []
  (let [things (rf/subscribe [:filtered-things])
        search (rf/subscribe [:search])]
    [:<>
     [:> sui/Header "Catalogue"]
     [:> sui/Form
      [:> sui/Form.Input
       {:label "search"
        :value (or (:q @search) "")
        :on-change #(rf/dispatch
                      [:set-search-q (-> % .-target .-value)])}]]
     [:> sui/Button {:on-click #(rf/dispatch [:clear-search])} "clear search"]
     [:> sui/List {:ordered true}
      (for [s @things] ^{:key s} [:> sui/List.Item s])]]))

(defn Detail
  []
  [:> sui/Header "Detail"])

(defn NotFound
  []
  [:> sui/Header "Not Found"])

(defn Link
  [url name]
  [:a {:href url} name])

(defn layout-ui
  []
  (let [route (rf/subscribe [:router/current-route])]
    [:div
     [:ul
      [:li [Link "/" "home"]]
      [:li [Link "/catalogue/" "catalogue"]]
      [:li [Link "/catalogue/?q=a%20test" "searching catalogue"]]
      [:li [Link "/catalogue/5.8.1/" "a detail"]]
      [:li [Link "/catalogue/4.8.7/?hl=A" "an detail with highlight"]]]
     (case @route
       :home [Home]
       :details/list [DetailList]
       :details/view-one [Detail]
       :error/not-found [NotFound]
       [:div "huh?"])
     [:> sui/Button {:on-click #(rf/dispatch [:initialize-db])} "reset db"]
     [:> sui/Button {:on-click #(rf/dispatch [:rehydrate-from-url])} "rehydrate from url"]
     [:> sui/Button {:on-click #(rf/dispatch [:set-route-programatically
                                              {:handler :details/view-one
                                               :route-params {:id "4.8.9"}}])}
      "change route programmatically"]]))

;;; kickoff

(defonce initializing? (atom true))

(defn mount-root
  []
  (when @initializing?
    (rf/clear-subscription-cache!)
    (rf/dispatch-sync [:initialize-db])
    (rf/dispatch [:rehydrate-from-url])
    (stylefy/init)
    (start-router!)
    (reset! initializing? false))
  (rg/render
    [:> sui/Container
     (stylefy/use-style {:margin-top "2em"} {})
     [layout-ui]]
    (.getElementById js/document "app")))

(mount-root)
