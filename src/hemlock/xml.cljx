(ns hemlock.xml
  #+clj
  (:require
   [clojure.zip :as z]
   [hemlock.core :as h])
  #+cljs
  (:require
   [clojure.zip :as z]
   [hemlock.core :as h :include-macros true]))

(defn xml-zip
  "A version of clojure.zip/xml-zip which uses :tag as the branch?
  predicate instead of (complement string?)."
  [root]
  (z/zipper :tag
            (comp seq :content)
            (fn [node children]
              (assoc node :content (and children (apply vector children))))
            root))

(def
  ^{:arglists '([root-node])}
  xml-builder
  "Return a function for constructing XML."
  (h/builder xml-zip))


(defn tag?
  "True if x is a map and has a truthy value for :tag."
  [x]
  (and (map? x) (:tag x)))


(defn attr
  [attr-key attr-val]
  (h/edit assoc-in [:attrs attr-key] attr-val))


(defn ex-tag-spec
  "Return an instance of ex-info whenever spec is invalid."
  [spec]
  (when-not (tag? (get-in spec [:node]))
    (ex-info "Invalid spec value."
             {:reason ":node property invalid"
              :expected {:tag '(not nil?)}
              :actual (:node spec)})))


(defmacro deftag
  "Define an xml-tag term function with spec."
  [sym spec]
  (let [sym (vary-meta sym assoc :arglists `'(~'[attrs? & content]))]
    `(do
       (when-let [ex# (ex-tag-spec ~spec)]
         (throw ex#))

       (h/defterm ~sym
         ~spec
         [& [attrs# & content#]]
         (if (and (map? attrs#)
                  (not (tag? attrs#)))
           (comp
            (h/edit update-in [:attrs] merge attrs#)
            (h/children content#))
           (h/children (cons attrs# content#)))))))
