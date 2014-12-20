(def dest-cards '(
	(3 {:type :inv :value 1})
	(1	{:type :exp :value 2}
			{:type :exp :value 3}
			{:type :exp :value 4}
			{:type :exp :value 5}
			{:type :exp :value 6}
			{:type :exp :value 7}
			{:type :exp :value 8}
			{:type :exp :value 9}
			{:type :exp :value 10})))

(def dest-types '(
	{:dest :yellow}
	{:dest :blue}
	{:dest :white}
	{:dest :green}
	{:dest :red}))

(defn single-set [times v1 & vn]
	"takes a count/entry and creates set from it"
	"ex. (single-set 3 {:type :inv :value 1} {:type :2 :value 2})"
	(mapcat #(repeat times %1) (cons v1 vn)))

(defn shuffled-deck []
	"creates the full deck of cards"
	(let [dest-set (mapcat #(apply single-set %1) dest-cards)]
		(shuffle (mapcat (fn [dest]
			(map #(merge dest %1) dest-set)) dest-types))))

(defn start-cards []
	"a map of :p1-hand :p2-hand :deck"
	(let [deck (shuffled-deck)]
		(let [player-cards (take 16 deck) rest-cards (drop 16 deck)]
			{	:p1-hand (take-nth 2 player-cards)
				:p2-hand (take-nth 2 (rest player-cards))
				:deck rest-cards})))

