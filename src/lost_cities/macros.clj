(in-ns 'lost-cities.core)

(defmacro =any [test v-1 & v-n]
	(cons 'or (map #(list = test %1) (cons v-1 v-n))))

(defmacro cond-validate [body ret & tests]
	`(cond ~@(mapcat #(list (first %1) `(do (println ~(second %1)) ~ret)) tests) :default ~body))
