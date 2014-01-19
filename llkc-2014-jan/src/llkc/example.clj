(ns llkc.example
  (:require [clara.rules :refer :all]
            [clara.rules.engine :as eng]
            [clara.rules.accumulators :as acc]
            [clara.tools.viz :as viz]
            [clojure.pprint :refer :all]
            [schema.core :as s]
            [clj-time.core :refer [now in-weeks interval date-time]]))

;; Work order.
(s/defrecord WorkOrder 
    [location :- s/String
     date :- org.joda.time.DateTime
     priority :- (s/enum :low :medium :high)])

;; Order line item.
(s/defrecord LineItem
    [type :- s/Keyword
     description :- s/String
     price :- s/Number])

;; Indication that approval is required for the order.
(s/defrecord ApprovalRequired 
    [reason :- (s/enum :restriction :timeline :price)
     description :- s/String])

;; Reference data indicating items of a given type are restricted at a given location.
(s/defrecord RestrictedItem 
    [type :- s/String
     location :- s/String])

;; Indication of a validation error.
(s/defrecord ValidationError 
    [reason :- s/String])

(defrule no-items
  "Valid orders must have at least one line item."
  [:not [LineItem]]
  =>
  (insert! (->ValidationError "No line items in order.")))

(defn weeks-until 
  "Returns the weeks between the current and given date."
  [date]
  (in-weeks (interval (now) date)))

;; Example rules.
(defrule date-too-early
  "We can't schedule something too early."
  [WorkOrder (< (weeks-until date) 2)]
  =>
  (insert! (->ApprovalRequired :timeline "Date is too early")))

(defrule budget-limit
  "There is a limit on the budget."
  [?total <- (acc/sum :price) :from [LineItem]]
  [:test (> ?total 1000)]
  =>
  (insert! (->ApprovalRequired :price "It's pricey!")))

(defrule restricted-items
  [WorkOrder (= ?location location)]
  [LineItem (= ?type type)]
  [RestrictedItem (= ?location location) (= ?type type)]
  =>
  (insert! (->ApprovalRequired :restriction 
                               (str ?type " is restricted at " ?location))))

(defquery needs-approval
  "Returns all approval requirements."
  []
  [?approval <- ApprovalRequired])

(defquery validation-errors
  "Returns all validation errors."
  []
  [?error <- ValidationError])

;;;;;;;;;;;;;;;;;;;
;; Example usage.
(comment

(def session
  (-> (mk-session 'llkc.example :cache false)
      (insert (->WorkOrder "KC" (date-time 2014 1 25) :high)
              (->LineItem :fan "Fan is broken." 600)
              (->LineItem :control-board "Control board is damaged" 500))
      (fire-rules)))

;; Query session for validation errors
(pprint (query session validation-errors))

;; Query session for approvals
(pprint (query session needs-approval))

;; Rules are data.
(pprint date-too-early)

(pprint budget-limit)

;; The network is data
(pprint (eng/to-beta-tree (eng/load-rules 'llkc.example)))

;; Show the network.
(viz/show-network! 'llkc.example)

;; Show the logic.
(viz/show-logic! 'llkc.example)

;; Select a subset of logic we're interested in! 
(viz/show-logic! 
 (filter 
  #(viz/inserts? % ValidationError)  
  (viz/get-productions ['llkc.example])))

;; Let's see what inserts or uses the approval required fact.
;; List comprehensions are great for finding interesting data.
(-> (for [production (viz/get-productions ['llkc.example])

          :when (or  (viz/inserts? production ApprovalRequired)
                     (viz/uses? production ApprovalRequired))]
      production)

    (viz/show-logic!))

;; Rules can be created from a data structure. Constraints stored
;; in external files or databases and read into the structure.
(-> [{:lhs [{:type llkc.example.WorkOrder
             :constraints '[(= ?location location) 
                            (= :high priority)]}]  
      :rhs '(println (str "High priority order for " ?location "!"))}]

    (mk-session)
    (insert (->WorkOrder "KC" (date-time 2014 1 25) :high))
    (fire-rules))

;; Immutable working memory.
(def s1
  (-> (mk-session 'llkc.example)
      (insert (->WorkOrder "KC" (date-time 2014 03 25) :high)
              (->LineItem :fan "Fan is broken." 600))
      (fire-rules)))

;; Query for approvals
(println (query s1 needs-approval))

;; Create a new sesion in s2.
(def s2 
  (-> s1 
      (insert (->LineItem :control-board "Control board is damaged" 500))
      (fire-rules)))

;; Query for approvals
(println (query s2 needs-approval))

;; Notice that s1 is unmodified! Allows for speculative processing and rollback.
(println (query s1 needs-approval))

;; We can take advantage of immutable working memory in other ways.
;; For example, we can load reference data from an external source.
(def ref-data [(->RestrictedItem  "Kryptonite" "FortressOfSolitude")
               (->RestrictedItem  "PhotonTorpedo" "DeathStar")
               ])

;; Create an immutable session with our reference data, and simply reuse it.
(def ref-session (insert-all (mk-session 'llkc.example :cache false) ref-data))

;; We can now reuse it rather than creating and loading a new session every time!
(pprint 
 (-> ref-session
     (insert (->WorkOrder "FortressOfSolitude" (date-time 2015 1 1) :high)
             (->LineItem "Kryptonite" "Surprise gift!" 0))
     (fire-rules)
     (query needs-approval)))

;; And the ref session itself is unmodified and can be used by others.
(pprint  (query ref-session needs-approval)))


