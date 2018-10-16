(ns sample.core
  "trying to keep one source of truth and have a search object
   derive the location bar, an input text and a search result.
   should change when:
   - we click a link in a started app
   - we change the input directly
   - we come in via a link directly"
  (:require [re-frame.core :as rf]
            [reagent.core :as rg]
            [bidi.bidi :as bidi]
            [pushy.core :as pushy]
            [semantic-ui-react :as sui]
            [stylefy.core :as stylefy]
            [cemerick.url]
            ; [cljs.core.async :refer [>! <! chan put! close! timeout]]
            [clojure.string]
            [goog.crypt.base64]
            [goog.Uri :as uri])
  (:import [goog.async Throttle]))

;; - route via a-href
;; - route via dispatch in form
;; - route restoration on reload
;; - set form value on route change
;; - set URL value on form change
;; - debounce pushy set-token!

(enable-console-print!)

(def interval 150)

(def app-routes
  ["/" [["" :home]
        ["catalogue/" {"" :details/list
                       [:id "/"] :details/view-one}]
        [true :error/not-found]]])

(defn encode-search
  [m]
  (let [ks (->> m keys (map name) (map cemerick.url/url-encode))
          vs (->> m vals (map cemerick.url/url-encode))
          result (->> (map vector ks vs) 
                      (map #(interpose "=" %))
                      (interpose "&")
                      flatten
                      clojure.string/join)]
      (str "?" result)))

(defn decode-search
  [s]
  )

(defn match-route
  "wrap bidi match-route* to make use of query params"
  [routes path & {:as options}]
  (let [route (bidi/match-route* routes path options)
        query-params (some-> path goog.Uri. .getQueryData)]
    (prn route path)
    (js/console.log "q=" (.getValues query-params "q")
                    "hl=" (.getValues query-params "hl"))
    route))

(def history
  (pushy/pushy (fn [match]
                 (let [{:keys [handler]} match]
                   (rf/dispatch [:set-route [handler]])))
               (partial match-route app-routes)))

(def start-router! #(pushy/start! history))

(defn update-url-bar*
  [m]
  (let [base-path (bidi/path-for app-routes :details/list)]
    (pushy/set-token! history
                      (str base-path (encode-search m)))))

(def update-url-bar (Throttle. update-url-bar* interval))

(rf/reg-fx
  :encode-search-into-url
  (fn [search]
    (.fire update-url-bar search)))

(def default-db
  {:route [:home]
   :search {}})

(rf/reg-event-db
 :initialize-db
 (fn [_ _] default-db))

(rf/reg-event-db
  :set-route
  (fn [db [_ route]]
    (assoc db :route route)))

(rf/reg-event-fx
  :set-search
  (fn [{:keys [db]} [_ search]]
    {:db (assoc db :search search)
     :encode-search-into-url search}))

(rf/reg-sub
  :router/current-route
  (fn [db _]
    (:route db)))

(rf/reg-sub
  :search
  (fn [db _]
    (:search db)))

(rf/reg-sub
  :db
  (fn [db _]
    db))

(defn Home
  []
  [:> sui/Header "Home route"])

(defn DetailList
  []
  [:> sui/Header "List"])

(defn Detail
  []
  [:> sui/Header "Detail"])

(defn NotFound
  []
  [:> sui/Header "Not Found"])

(defn Link
  [url name]
  [:a {:href url} name])

(defn window-location
  []
  (-> js/window .-location .-href))

(defn are-all-congruent?
  []
  (let [location-bar (window-location) 
        app-db @(rf/subscribe [:search])]
    (prn location-bar)
    (= location-bar app-db)))

(defn Test
  []
  (if (are-all-congruent?)
    [:> sui/Label {:color "green"} "Passing"]
    [:> sui/Label {:color "red"} "Failing"]))

(defn layout-ui
  []
  (let [route (rf/subscribe [:router/current-route])
        search (rf/subscribe [:search])
        db (rf/subscribe [:db])]
    [:div
     [:> sui/Form
      [:> sui/Form.Input
       {:label "search"
        :value (or (:q @search) "")
        :on-change #(rf/dispatch [:set-search {:q (-> % .-target .-value)}])}]]
     [:> sui/Divider {:hidden true}]
     [:> sui/Button {:on-click #(rf/dispatch [:initialize-db])} "reset db"]
     [:> sui/Button {:on-click #(rf/dispatch [:set-route [:details/view-one]])} "change route"]
     (case @route
       [:home] [Home]
       [:details/list] [DetailList]
       [:details/view-one] [Detail]
       [:error/not-found] [NotFound]
       [:div "huh?"])
     [:ul
      [:li [Link "/" "home"]]
      [:li [Link "/catalogue/" "catalogue"]]
      [:li [Link "/catalogue/?q=a%20test" "searching catalogue"]]
      [:li [Link "/catalogue/5.8.1/" "a detail"]]
      [:li [Link "/catalogue/4.8.7/?hl=A" "an detail with highlight"]]]
     [:> sui/Segment 
      [:> sui/Header "app-db"]
      [:code (str @db)]
      [:> sui/Header "location"]
      [:code (window-location)]]
     [Test]]))

(defonce initializing? (atom true))

(defn mount-root
  []
  (when @initializing?
    (rf/clear-subscription-cache!)
    (rf/dispatch-sync [:initialize-db])
    (stylefy/init)
    (start-router!)
    (reset! initializing? false))
  (rg/render
    [:> sui/Container
     (stylefy/use-style {:margin-top "2em"} {})
     [layout-ui]]
    (.getElementById js/document "app")))

(mount-root)
