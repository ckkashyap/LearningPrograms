(require 'cljs.build.api)

(cljs.build.api/watch "src"
                      {:main 'hello-world.core
                       :optimizations :advanced
                       :output-to "out/main.js"})
