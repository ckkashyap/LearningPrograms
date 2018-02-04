(ns hello-world.core)

(enable-console-print!)

(println "Hello world!")

(defn ^:export hello []
(js/alert "hello 123"))


