(ns hello-world.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require
   [goog.structs.Map :as goog.structs.Map]
   [goog.json :as gjson]
   [goog.net.XhrManager :as manager]
   [cljs.core.async :as async :refer [chan close!]]
   [goog.net.XhrIo :as xhr]))
      

(enable-console-print!)

(println "Hello world!")


; copied from https://github.com/hozumi/simple-xhr

(def xhr-manager
  (atom (goog.net.XhrManager. js/undefined
                              js/undefined
                              js/undefined
                              js/undefined
                              js/undefined)))

(defn request
  "Asynchronously make a network request for the resource at url. If
  provided via the `:on-success` and `:on-error` keyword arguments, the
  appropriate one of `on-success` or `on-error` will be called on
  completion. They will be passed a map containing the keys `:id`,
  `:body`, `:status`, and `:event`. The entry for `:event` contains an
  instance of the `goog.net.XhrManager.Event`.
  Other allowable keyword arguments are `:method`, `:content`, `:headers`,
  `:priority`, and `:retries`. `:method` defaults to \"GET\" and `:retries`
  defaults to `0`."
  [& {:keys [url id method content json headers priority retries
             complete success error complete-after]
      :or   {method   "GET"
             retries  0}}]
  (let [headers (if json (assoc headers "Content-Type" "application/json")
                    headers)
        content (if json
                  (-> json clj->js gjson/serialize) content)]
    (.send @xhr-manager
           (or id url)
           url
           method
           content
           (and headers (goog.structs.Map. (clj->js headers)))
           priority
           (fn [e] (let [xhrio (.-target e)]
                     (when complete
                       (complete xhrio))
                     (if (.isSuccess xhrio)
                       (when success
                         (success xhrio))
                       (when error
                         (error xhrio)))
                     (when complete-after
                       (complete-after xhrio))))
           retries)))

(defn GET [url]
  (let [ch (chan 1)]
    (xhr/send url
              (fn [event]
                (let [res (-> event .-target .getResponseText)]
                  (go (>! ch res)
                      (close! ch)))) "POST" "123aaaa4" (goog.structs.Map.  (clj->js {"Content-Type" "application/json"})))
    ch))

(defn ^:export hello []
  (let [
        div (.getElementById js/document "main_div")
        html (.-innerHTML div)
        _ (println html)
        _ (set! (.-innerHTML div) (str html html))
        m {"A" 10 "B" 20}
        _ (println (clj->js m))
                                        ;_   (js/alert "hello 123")
                                        ;_ (GET "http://localhost:9090/DingoBingo")
        _ (request :url "http://www.google.com"
                   :headers {"A" "20"})
        
        ]
    (println "hello called")))
