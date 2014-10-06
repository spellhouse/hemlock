(ns hemlock.core
  (:require
   [clojure.zip :as z])
  #+cljs
  (:require-macros
   [hemlock.core :refer [defcurried]]))

;; ---------------------------------------------------------------------
;; Utilities

;;; Macro helpers

#+clj
(defn ^:private munge-arglist
  "Given an argument list create a new one with generated symbols."
  [arglist]
  (vec (for [arg arglist]
         (if (= '& arg)
           arg
           (gensym "a_")))))

#+clj
(defn ^:private normalize-arglist
  "Removes variation from an argument list."
  [arglist]
  (vec (remove '#{&} arglist)))

#+clj
(defn ^:private variadic?
  "Returns true if arglist is variadic (contains &), 
  false otherwise."
  [arglist]
  (boolean (some '#{&} arglist)))

;;; Currying

#+clj
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
  {:arglists '([name docstring? [params*] & body])}
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
  (if (z/branch? loc)
    (z/append-child loc x)
    (z/insert-left loc x)))


(defcurried children
  [xs loc]
  (applicate loc (for [x xs]
                   (cond
                     (fn? x)
                     x

                     (sequential? x)
                     (children x)

                     :else
                     (child x)))))


(defn edit
  "Given a fn f and variable number of arguments return a function which
  takes a zipper and applies zip/edit to it with f and args"
  [f & args]
  (fn [loc]
    (apply z/edit loc f args)))


(defcurried builder
  "Apply a variable number of edits to the zipper created by 
  (zipper root-node) and return it's value."
  [zipper root-node]
  (fn [& edits]
    (z/root (children edits (zipper root-node)))))


;; ---------------------------------------------------------------------
;; Term functions

(defn make-term
  "Return a function f which accepts a variable number of unary 
  functions which operate on a zipper and returns a unary function g of
  a zipper that appends tag and applicates edits to it. Before g returns
  the editted tag may be validated with validator. g returns focus to the
  to the original location."
  [{:keys [node pre post]
    :or {pre identity
         post identity}}]
  (fn f [& edits]
    (fn g [ploc]
      ;; Run a pre operation on the parent node before applying edits.
      (pre (z/node ploc))
      (let [cloc (-> ploc
                     (z/append-child node)
                     (z/down)
                     (z/rightmost)
                     (applicate edits))]
        ;; Run post operations on the child node after it has been
        ;; appended.
        (post (z/node cloc))
        ;; Return the parent location.
        (z/up cloc)))))


(defmacro term
  "Returns a term function whose return value is passed to the function 
  returned by (make-term spec). The term function must return a function
  which accepts a zipper and returns a zipper.
  
  Ex.

    (let [span (term
                {:node {:tag :span :content []}}
                [& subnodes]
                (children subnodes))]
      (->> (z/xml-zip {:tag :body :content []})
           ((span 1 2 3))
           (z/root)))
    ;; => {:tag :body, :content [{:tag :span, :content [1 2 3]}]}
  "
  [spec & fn-tail]
  (let [fn-tail (if (list? (first fn-tail))
                  fn-tail
                  (list fn-tail))
        tsym (gensym "term")
        fsym (gensym "f")]
    `(let [~tsym (make-term ~spec)
           ~fsym (fn ~@fn-tail)]
       (fn ~@(for [[args & fn-body] fn-tail]
               (let [margs (munge-arglist args)
                     nargs (normalize-arglist margs)]
                 `(~margs
                   (~tsym ~(if (variadic? args)
                             `(apply ~fsym ~@nargs)
                             `(~fsym ~@nargs))))))))))


(defmacro defterm
  "Define a term function by name."
  [name spec & fn-tail]
  `(def ~name (term ~spec ~@fn-tail)))
