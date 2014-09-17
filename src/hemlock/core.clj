(ns hemlock.core
  (:require
   [clojure.zip :as z]))

;; ---------------------------------------------------------------------
;; Utilities

;;; Currying

(defn ^:private normalize-arglist
  "Removes variation from an argument list."
  [arglist]
  (vec (remove '#{&} arglist)))

(defn ^:private variadic?
  "Returns true if arglist is variadic (contains &), 
  false otherwise."
  [arglist]
  (boolean (some '#{&} arglist)))

(defn ^:private do-curried
  "Helper function for defcurried."
  [args form]
  (let [[sargs vargs] (split-with #(not= % '&) args)]
    (letfn [(curry [a b]
              (if (seq b)
                `(~(vec a) ~(curried b))
                `(~(vec (concat a vargs)) ~form)))
            (curried [args]
              (cons 'fn (for [i (range 1 (inc (count args)))
                              :let [a (take i args)
                                    b (drop i args)]]
                          (curry a b))))]
      (curried sargs))))

(defmacro defcurried
  "Define a curried function."
  {:arglists '([name docstring? arglist & body])}
  [name & body]
  (let [docstring (when (string? (first body))
                    (first body))
        body (if docstring
               (next body)
               body)
        arglist (first body)
        body (next body)
        name (if docstring
               (vary-meta name assoc :doc docstring)
               name)
        name (vary-meta name assoc :arglists `'(~arglist))
        nargs (count arglist)]
    (if (or (<= nargs 1)
            (and (variadic? arglist)
                 (<= nargs 3)))
      `(def ~name (fn ~arglist ~@body))
      (let [fsym (symbol (str name "-curried"))
            form (if (variadic? arglist)
                   `(apply ~fsym ~@(normalize-arglist arglist))
                   `(~fsym ~@arglist))]
        `(def ~name
           (let [~fsym (fn ~arglist ~@body)]
             ~(do-curried arglist form)))))))


(defcurried ^:private applicate
  "Applies the composition of fs to x."
  [x fs]
  (reduce (fn [x' f] (f x')) x fs))


;; ---------------------------------------------------------------------
;; Location functions

(def pass
  (constantly identity))

(defcurried child
  "Like clojure.zip/append-child but takes it's arguments in reverse."
  [x loc]
  (z/append-child loc x))

(defcurried children
  [xs loc]
  (applicate loc (for [x xs] (if (fn? x) x (child x)))))

(defcurried attr
  [attr-name attr-val loc]
  (z/edit loc assoc-in [:attrs attr-name] attr-val))


;; ---------------------------------------------------------------------
;; Term

(defn make-term
  "Return a function f which accepts a variable number of unary 
  functions which operate on a zipper and returns a unary function g of
  a zipper that appends tag and applicates edits to it. Before g returns
  the editted tag may be validated with validator. g returns focus to the
  to the original location."
  [{:keys [tag validator] :or {validator identity}}]
  (fn f [& edits]
    (fn g [ploc]
      (let [cloc (-> ploc
                     (z/append-child tag)
                     (z/down)
                     (z/rightmost)
                     (applicate edits))]
        ;; Validate the tag.
        (validator (z/node cloc))
        ;; Return the parent location.
        (z/up cloc)))))


(defmacro term [spec & fn-tail]
  (let [fn-tail (if (list? (first fn-tail))
                  fn-tail
                  (list fn-tail))
        tsym (gensym "term")
        fsym (gensym "f")]
    `(let [~tsym (make-term ~spec)
           ~fsym (fn ~@fn-tail)]
       (fn ~@(for [[spec & fn-body] fn-tail]
               `(~spec
                 (~tsym ~(if (variadic? spec)
                           `(apply ~fsym ~@(normalize-args spec))
                           `(~fsym ~@(normalize-args spec))))))))))

(defmacro defterm
  [name spec & fn-tail]
  `(def ~name (term ~spec ~@fn-tail)))
