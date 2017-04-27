 (ns user
   (:require
     [cljs.repl :as repl]
     [ambly.core :as ambly]))

 (defn connect
   []
   (repl/repl (ambly/repl-env) :analyze-path "src"))
