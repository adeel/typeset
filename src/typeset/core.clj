(ns typeset.core
  (:use [clojure.set :only (union difference)])
  (:require [typeset.html :as html]))

;; units

(defn- make-text-unit
  "Idempotent function that returns a text unit given a string."
  [text]
  (if (= (type text) java.lang.String)
    {:type  :text
     :body  text}
    text))

(defn text
  "Returns a text list whose elements are the given arguments turned into text
  units."
  [& ts]
  {:type :text-list
   :body (map make-text-unit ts)})

(defn para
  "Returns a paragraph containing the given arguments turned into text units."
  [& ts]
  {:type :paragraph
   :body (map make-text-unit ts)})

(defn claim
  "Returns a claim unit whose title is the first argument, and body is formed
  by the remaining arguments.  Generally the title should be a text list and
  the body should consist of paragraphs."
  [title & body]
  {:type    :claim
   :title   title
   :body    body})

;; formatting

(defn- make-format-toggler [f]
  (fn [text]
    (let [text    (make-text-unit text)
          formats (get text :formats #{})]
      (assoc text :formats (if (contains? formats f)
                             (difference formats #{f})
                             (union formats #{f}))))))

(def ^{:doc "Takes a text unit or string and toggles its bold formatting."}
  bold (make-format-toggler :bold))
(def ^{:doc "Takes a text unit or string and toggles its italic formatting."}
  italic (make-format-toggler :italic))
(def ^{:doc "Takes a text unit or string and toggles its math parsing flag.
             If the flag is set, the text will be parsed as math using LaTeX."}
  math (make-format-toggler :math))

;; rendering

(defn render
  "Render the given claims using the given method (currently only :html
  is implemented) and options (:numbering-prefix can be a prefix to add to
  the claim numbers)."
  [method options & claims]
  (case method
    :html (html/render claims (options :numbering-prefix))))

(comment
  (render :html {:numbering-prefix "1.1."}
    (claim (text "The open subsets of a topological space " (math "X") " form a category "
                 (math "\\mathrm{Top}(X)") ".")
           (para "We have as morphisms the inclusion maps " (math "U \\to V")
                 " for any inclusions " (math "V \\subseteq U") ", "
                 (math "U") " and " (math "V") " open subsets of X.")
           (para "Another paragraph."))
    (claim (text "This is the second claim.")
           (para "This is in the body of the second claim.")
           (para "And this is another paragraph."))))
