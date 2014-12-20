(in-ns 'lost-cities.core)

(def player-types {"p" :player "c" :ai})
(def game-prompt "game")

(defn c-hand [state]
	"current player hand"
	(state (keyword (str (name (state :turn)) "-hand"))))

(defmulti game-command (fn [cmd args state] cmd))

(defmethod game-command :default [cmd _ state]
	(println "unrecognized command:" cmd)
	state)

(defmethod game-command "exit" [_ _ _] nil)
(defmethod game-command "quit" [_ _ _] nil)
(defmethod game-command "done" [_ _ _] nil)

(defmethod game-command "new-game" [_ args state]
	(let [p1 (player-types (first args))
				p2 (player-types (second args))]
		(cond-validate
			(do
				(println "starting new game")
				(merge 	(start-cards)
								{:prompt game-prompt :p1 p1 :p2 p2 :turn :p1}))
			state
			((=any nil p1 p2) "syntax: new-game <p|c> <p|c>")
			((= p1 p2 :player) "player vs player not supported"))))

(defmethod game-command "show-state" [_ args state]
	(println (pr-str (state (keyword (first args)))))
	state)

(defmethod game-command "show-hand" [_ _ state]
	(println (pr-str (sort-by (juxt :dest :value) (c-hand state))))
		state)

(defn validate-card [dest value hand]
	"validates whether the card is valid and the player has the card"
	(println "you want to play" dest value)
	(cond
		(=any nil  dest value)
		(println "card not in correct format <dest-value>")
		(not (or (number? value) (= "inv" value)))
		(println "value not correct (1-10 | inv)")
		:default
		(let [dest (keyword dest)
					value (#(if (= "inv" %1) 1 (read-string %1)) value)]
			(let [hand-group (group-by #(and (= dest (%1 :dest)) (= value (%1 :value))) hand)
						match (hand-group true)
						play-card (first match)
						hand-left (cons (second match) (hand-group false))]
				(println "play card" play-card)
				(println "remaining hand" hand-left)))))

(defn turn-action [card gamestate]
	"handles the action portion of the turn"
	(if (= nil card)
		(do
			(println "must provide a card")
			gamestate)
		(do
			(let [c-split (str/split card #"-")]		
				(validate-card (first c-split) (second c-split) (c-hand gamestate))
				(let [player (gamestate :turn)]
					(merge gamestate {:turn (if (= player :p1) :p2 :p1)}))))))

(defmethod game-command "play" [_ args state]
	(turn-action (first args) state))

(defmethod game-command "discard" [_ args state]
	(turn-action (first args) state))
