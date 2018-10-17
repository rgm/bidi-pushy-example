(ns sample.handler
  (:require [ring.util.response
             :refer [resource-response content-type not-found]]))

(defn handler
  [req]
  (not-found "Not found"))

(defn spa-handler
  [req]
  (prn "running ring handler")
  (or (when true
        (some-> (resource-response "index.html" {:root "public"})
                (content-type "text/html; charset=utf-8")))
      (not-found "Not found")))
