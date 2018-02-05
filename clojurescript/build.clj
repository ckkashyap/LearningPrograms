(require 'cljs.build.api)

(cljs.build.api/build "src"
                      {:main 'hello-world.core
                       :optimizations :advanced
                       :output-to "out/main.js"})
