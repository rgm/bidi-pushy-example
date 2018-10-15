(ns sample.core
  (:require [re-frame.core :as rf]
            [reagent.core :as rg]
            [bidi.bidi :as bidi]
            [pushy.core :as pushy]
            [semantic-ui-react :as sui]
            [stylefy.core :as stylefy]
            [cemerick.url]
            [goog.crypt.base64]
            [goog.Uri :as uri]))

;; - route via a-href
;; - route via dispatch in form
;; - route restoration on reload

(enable-console-print!)

(def app-routes
  ["/" [["" :home]
        ["catalogue/" {"" :details/list
                       [:id "/"] :details/view-one}]
        [true :error/not-found]]])

(defn encode-search
  [m]
  (.encodeString goog.crypt.base64 (str m)))

(defn decode-search
  [s]
  )

(defn match-with-params
  [routes path & {:as options}]
  (let [route (bidi/match-route* routes path options)
        query-params (some-> path goog.Uri. .getQueryData)
        fragment (some-> path goog.Uri. .getFragment)]
    (prn route)
    (js/console.log "query" query-params)
    (js/console.log "fragment" fragment)
    route))

(def history
  (pushy/pushy (fn [match]
                 (let [{:keys [handler]} match]
                   (rf/dispatch [:set-route [handler]])))
               (partial match-with-params app-routes)))

(def start-router! #(pushy/start! history))

(rf/reg-fx
  :encode-search-into-url
  (fn [search] (prn "encode!" search)))

(def default-db
  {:route [:home]})

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

(defn Home
  []
  [:div "Home route"])

(defn DetailList
  []
  [:div "List"])

(defn Detail
  []
  [:div "Detail"])

(defn NotFound
  []
  [:div "Not Found"])

(defn Link
  [url name]
  [:a {:href url} name])

(defn layout-ui
  []
  (let [route (rf/subscribe [:router/current-route])]
    [:div
     [:> sui/Button {:on-click #(start-router!)} "start router"]
     [:> sui/Button {:on-click #(rf/dispatch [:initialize-db])} "reset db"]
     [:> sui/Button {:on-click #(rf/dispatch [:set-route [:details/view-one]])} "change route"]
     (case @route
       [:home] [Home]
       [:details/list] [DetailList]
       [:details/view-one] [Detail]
       [:error/not-found] [NotFound]
       [:div "huh?"])
     [:> sui/Segment (str @route)]
     [:> sui/Form
      [:> sui/Form.Input
       {:label "search"
        :default-value "yo"
        :on-change #(rf/dispatch [:set-search (-> % .-target .-value)])
        }]]
     [:ul
      [:li [Link "/" "home"]]
      [:li [Link "/catalogue/" "catalogue"]]
      [:li [Link "/catalogue/?text=hi" "searching catalogue"]]
      [:li [Link "/catalogue/detail-id/" "a detail"]]
      [:li [Link "/catalogue/detail-id/#anchor" "an anchored detail"]]
      [:li [Link "/catalogue/detail-id/?hl=what#anchor"
       "an anchored detail with highlight"]]
      ]]))

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
