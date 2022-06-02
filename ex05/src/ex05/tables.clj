(ns ex05.tables)


(defn init-class-tbl [class-name dict]
  (-> (assoc (dict :class_tbl)
             :name class-name
             :static_symb 0
             :field_symb 0)
      (#(assoc dict :class_tbl %))))

(defn add-static [type id dict]
  (->> {:type type :kind "static"
        :count ((dict :class_tbl) :static_symb)}
       (assoc (dict :class_tbl) id)
       (#(assoc % :static_symb (+ (% :static_symb) 1)))
       (#(assoc dict :class_tbl %))))

(defn add-field [type id dict]
  (->> {:type type :kind "this"
        :count ((dict :class_tbl) :field_symb)}
       (assoc (dict :class_tbl) id)
       (#(assoc % :field_symb (+ (% :field_symb) 1)))
       (#(assoc dict :class_tbl %))))


(defn init-func-tbl [sub-type ret-type name dict]
  (-> (assoc (dict :func_tbl)
             :sub_type sub-type
             :ret_type ret-type
             :name name
             :arg_symb 0
             :var_symb 0)
      (#(assoc dict :func_tbl %))))

(defn add-arg [type id dict]
  (->> {:type type :kind "argument"
        :count ((dict :func_tbl) :arg_symb)}
       (assoc (dict :func_tbl) id)
       (#(assoc % :arg_symb (+ (% :arg_symb) 1)))
       (#(assoc dict :func_tbl %))))

(defn add-var [type id dict]
  (->> {:type type :kind "local"
        :count ((dict :func_tbl) :var_symb)}
       (assoc (dict :func_tbl) id)
       (#(assoc % :var_symb (+ (% :var_symb) 1)))
       (#(assoc dict :func_tbl %))))

(defn get-var-type [var dict]
  (get ((dict :func_tbl) var) :type (get ((dict :class_tbl) var) :type nil))) ;

(defn get-var-kind [var dict]
  (get ((dict :func_tbl) var) :kind (get ((dict :class_tbl) var) :kind nil)))

(defn get-var-count [var dict]
  (get ((dict :func_tbl) var) :count (get ((dict :class_tbl) var) :count nil)))


(defn pop-var [var dict]
  (str "pop "
       (get-var-kind var dict) " "
       (get-var-count var dict)))

(defn push-var [var dict]
  (str "push "
       (get-var-kind var dict) " "
       (get-var-count var dict)))

(defn get-function-dec [dict]
  (str "function "
       ((dict :class_tbl) :name) "."
       ((dict :func_tbl) :name) " "
       ((dict :func_tbl) :var_symb)))
