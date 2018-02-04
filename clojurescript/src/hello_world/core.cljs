(ns hello-world.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require
   [cljs.core.async :as async :refer [chan close!]]
   [goog.net.XhrIo :as xhr]))
      

(enable-console-print!)

(println "Hello world!")

(defn ^:export hello []
  (let [
        div (.getElementById js/document "main_div")
        html (.-innerHTML div)
        _ (println html)
        _ (set! (.-innerHTML div) (str html html))
        ;_   (js/alert "hello 123")
        
        ]
    (println "hello called")))
