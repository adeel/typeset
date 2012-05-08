(ns typeset.html
  (:use [clojure.string :only (join)]))

(defmulti render-node :type)

(defn- apply-format [html f]
  (case f
    :bold   (str "<b>" html "</b>")
    :italic (str "<i>" html "</i>")
    :math   (str "<span class=\"math\">" html "</span>")
    html))

(defmethod render-node :text [t]
  (reduce apply-format (t :body) (t :formats)))

(defmethod render-node :text-list [ts]
  (join (map render-node (ts :body))))

(defmethod render-node :paragraph [p]
  (str "<p>" (join (map render-node (p :body))) "</p>"))

(defmethod render-node :claim [c [claims numbering-prefix]]
  (str "<h3>"
       "§" numbering-prefix (inc (.indexOf claims c)) ". " (render-node (c :title))
       "</h3>"
       (join (map render-node (c :body)))))

(defn render [claims numbering-prefix]
  (join (map #(render-node % [claims numbering-prefix]) claims)))
