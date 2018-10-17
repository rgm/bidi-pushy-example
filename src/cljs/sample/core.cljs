(ns sample.core
  "trying to keep one source of truth and have a search object
   derive the location bar, an input text and a search result.
   should change when:
   - we click a link in a started app
   - we change the input directly
   - we come in via a link directly"
  (:require [bidi.bidi :as bidi]
            [cemerick.url]
            [cljs.core.match :refer [match]]
            [pushy.core :as pushy]
            [re-frame.core :as rf]
            [reagent.core :as rg]
            [clojure.walk :refer [keywordize-keys]]
            [semantic-ui-react :as sui]
            [stylefy.core :as stylefy]
            [taoensso.timbre :refer-macros [debug warn spy]])
  (:import [goog Uri]
           [goog.async Throttle]))

(def throttle-interval 150)

(def app-routes
  ["/"
   [["" :home]
    ["catalogue/"
     {"" :details/list
      [:id "/"] :details/view-one}]
    [true :error/not-found]]])

(defn pushy->bidi
  "converts pushy's output to something suitable for use in bidi/path-for"
  [m]
  (let [{:keys [handler route-params]} m]
    (reduce (fn [acc [k v]]
              (conj acc k v)) [handler] route-params)))

(defn match-route
  "wrap bidi match-route* to make use of query params"
  [routes path & {:as options}]
  (let [route (bidi/match-route* routes path options)
        query (some-> path Uri. .getQuery cemerick.url/query->map keywordize-keys)]
    (assoc route :query-params query)))

(def history
  (pushy/pushy
   (fn [match]
     (rf/dispatch [:set-pushy-route (pushy->bidi match)])
     (rf/dispatch [:rehydrate-from-url]))
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
    (str loc (some->> (cemerick.url/map->query m)
                      (str "?")))))

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
   :things ["hi"
            "whatup"
            "things are good"
            "some test data"]})

(rf/reg-cofx
 :window-location
 (fn [cofx _]
   (assoc cofx :window-location
          (-> js/window .-location .-href))))

(rf/reg-fx
 :encode-search-into-url
 (fn [[current-location search]]
   (.fire update-url-bar current-location search)))

;;; the pushstate effect will kick off whatever other events need
;;; kicking off via pushy's matching and dispatching
(rf/reg-fx
 :push-state
 (fn [route]
   (let [path (apply bidi/path-for app-routes route)]
     (pushy/set-token! history path))))

(rf/reg-event-db
 :initialize-db
 [rf/debug]
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
 [rf/debug (rf/inject-cofx :window-location)]
 (fn [{:keys [window-location db]} _]
   (let [search (extract-search window-location)]
     {:db (assoc db :search search)})))

;;; pushstate only otherwise we're gonna infinite loop
(rf/reg-event-db
 :set-pushy-route
 [rf/debug]
 (fn [db [_ route]]
   (assoc db :route route)))

;;; let pushstate dispatch take over if we change via an event
(rf/reg-event-fx
 :set-route-programatically
 [rf/debug]
 (fn [_ [_ route]]
   {:dispatch [:set-pushy-route route]
    :push-state route}))

(rf/reg-event-fx
 :set-search-q
 [rf/debug (rf/inject-cofx :window-location)]
 (fn [{:keys [window-location db]} [_ search-text]]
   (let [encoded-search (assoc (:search db) :q search-text)]
     {:db (assoc db :search encoded-search)
      :encode-search-into-url [window-location encoded-search]})))

(rf/reg-event-fx
 :clear-search
 [rf/debug (rf/inject-cofx :window-location)]
 (fn [{:keys [window-location db]} _]
   {:db (dissoc db :search)
    :encode-search-into-url [window-location nil]}))

(rf/reg-sub
 :router/current-route
 (fn [db _]
   (:route db)))

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
  [:<>
   [:> sui/Header "Home route"]
   [:p "(Home page stuff)"]])

(defn DetailList
  []
  (let [things (rf/subscribe [:filtered-things])
        search (rf/subscribe [:search])]
    [:<>
     [:> sui/Header "Catalogue"]
     [:> sui/Form
      [:> sui/Form.Input
       {:label "search"
        :type "search"
        :placeholder "search..."
        :value (or (:q @search) "")
        :on-change #(rf/dispatch
                     [:set-search-q (-> % .-target .-value)])}]]
     [:> sui/Button {:basic true :on-click #(rf/dispatch [:clear-search])} "clear search"]
     [:> sui/List {:ordered true}
      (for [s @things] ^{:key s} [:> sui/List.Item s])]]))

(defn OneDetail
  [id]
  [:> sui/Header "Detail " id])

(defn NotFound
  []
  [:> sui/Header "Not Found"])

(defn Link
  [url name]
  [:a {:href url} [:strong name] " " url])

(defn layout-ui
  []
  (let [route (rf/subscribe [:router/current-route])]
    [:div
     [:ul
      [:li [Link "/" "home"]]
      [:li [Link "/catalogue/" "full catalogue"]]
      [:li [Link "/catalogue/?q=are%20good" "saved catalogue search"]]
      [:li [Link "/catalogue/5.8.1/" "a detail"]]
      [:li [Link "/catalogue/4.8.7/?hl=A" "an detail with highlight"]]]
     (match @route
       [:home] [Home]
       [:details/list] [DetailList]
       [:details/view-one :id detail-ref] [OneDetail detail-ref]
       [:error/not-found] [NotFound]
       :else [:div "huh? no route match"])
     [:> sui/Divider]
     [:> sui/Button.Group {:basic true :vertical true}
      [:> sui/Button {:on-click #(rf/dispatch [:initialize-db])} "reset db"]
      [:> sui/Button {:on-click #(rf/dispatch [:rehydrate-from-url])} "rehydrate from url"]]
     [:> sui/Divider {:hidden true}]
     [:> sui/Button.Group {:basic true :vertical true}
      [:> sui/Button {:on-click
                      #(do
                         (rf/dispatch [:set-route-programatically [:details/list]])
                         (rf/dispatch [:clear-search]))}
       "show detail list"]
      [:> sui/Button {:on-click
                      #(do (rf/dispatch [:set-route-programatically
                                         [:details/list]])
                           (rf/dispatch [:set-search-q "wha"]))}
       "show detail list with search"]
      [:> sui/Button {:on-click
                      #(rf/dispatch [:set-route-programatically
                                     [:details/view-one :id "4.8.9"]])}
       "show detail 4.8.9"]]]))

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
