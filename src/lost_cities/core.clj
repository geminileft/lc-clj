(ns lost-cities.core)
(require '[clojure.string :as str])
(load "macros")
(load "cards")
(load "ai")
(load "core_commands")

(defn show-prompt [gst]
	"displays the prompt and optionally the current (p)layer"
	(print (gst :prompt))
	(when-let [p (gst :turn)] (print p))
	(print ">")
	(flush))

(defn get-input [gst]
	"get input from player or ai"
	(let [p (gst :turn)]
		(if (= (gst p) :ai)
			(ai-input gst)
			(read-line))))

(defn -main []
	(with-local-vars [gst {:prompt "?"}]
		(show-prompt @gst)
		(loop [input (read-line)]
			(let [split (str/split input #" ")
						cmd (first split)
						args (rest split)]
				(var-set gst 
					(game-command cmd args @gst)))
			(if (not= @gst nil)
				(do
					(show-prompt @gst)
					(recur (get-input gst)))
				(println "Exiting")))))
