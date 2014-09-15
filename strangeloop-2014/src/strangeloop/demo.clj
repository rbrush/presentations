(ns strangeloop.demo
  (:require [clara.rules :refer :all]
            [clara.rules.compiler :as com]
            [clara.rules.accumulators :as acc]
            [clara.tools.viz :as viz]
            [clara.tools.inspect :as inspect]
            [clara.tools.ui :as ui]
            [clojure.pprint :refer [pprint]]
            [schema.core :as s]
            [clj-time.core :as t])
  (:import [org.joda.time LocalDate]))


;;;;;;;;;;;;;;;;;;
;; Supporting functions
(defn age [birth-date]
  (t/in-years (t/interval birth-date (t/now))))

;;;;;;;;;;;;;;;;;;
;; Records used by the demo.

(s/defrecord BloodPressure
    [systolic :- s/Int
     diastolic :- s/Int])

(s/defrecord Hypertensive
    [severity :- (s/enum :mild :moderate :severe)])

(s/defrecord RecommendedMeds
    [type :- (s/enum :lisinopril)])

(s/defrecord Demographics
    [name :- s/Str
     birth-date :- LocalDate])

(s/defrecord ChronicKidneyDisease [])

(s/defrecord Referral [type :- (s/enum :nephropathy)])

;;;;;;;;;;;;;;;;;;;
;; Demo rules.

(defrule mild-hypertensive
  "Detect mild hypertension."
  [BloodPressure (> 140 systolic 120)]
  =>
  (insert! (->Hypertensive :moderate)))

(defrule senior-bp-meds
  "Recommend appropriate medication for hypertensive patients."
  [Hypertensive]
  [Demographics (> (age birth-date) 60)]
  =>
  (insert! (->RecommendedMeds :lisinopril)))

(defrule bp-with-chronic-kidney
  "Hypertensive with chronic kidney should be actively managed."
  [Hypertensive]
  [ChronicKidneyDisease]
  =>
  (insert! (->Referral :nephropathy)))

(defquery recommended-meds
  "Recommend meds query."
  []
  [RecommendedMeds (= ?type type)])

(defquery recommended-referrals
  "Returns recommended referrals."
  []
  [Referral (= ?type type)])

;;;;;;;;;;;;;;;;;;;
;; Example usage.

(comment

;;;;;;;;;;;;;;;;;;;
;; Rules Demo
(pprint (-> (mk-session)
            (insert (->Demographics "Alice"
                                    (-> 70 t/years t/ago))
                    (->BloodPressure 130 100))
            (fire-rules)
            (query recommended-meds)))

;; Sessions are immutable.
(def s1 (-> (mk-session)
            (insert (->Demographics "Alice"
                                    (-> 70 t/years t/ago)))
            (fire-rules)))

;; Nothing matches.
(pprint (query s1 recommended-meds))


;; Inserting a new fact creates a new, immutable session.
(def s2 (-> s1
            (insert (->BloodPressure 130 100))
            (fire-rules)))

;; Our rule now matches
(pprint (query s2 recommended-meds))

;; But the original is unchanged.
(pprint (query s1 recommended-meds))

;; Can specify multiple namespaces and create a constant session for speed and simplicity.
(def empty-session (mk-session 'strangeloop.demo))

;; A macro for convenience:
(defsession empty-session 'strangeloop.demo)

;; And use that in a normal function.

(defn get-medications
  "Accepts a sequence of the person's medical history and
   returns a sequence of recommended medication types."
  [person-data]
  (let [person-session (-> empty-session
                           (insert-all person-data)
                           (fire-rules))]

    (for [{?type :?type} (query person-session recommended-meds)]

      ?type)))


(pprint
 (get-medications [(->Demographics "Alice"
                                   (-> 70 t/years t/ago))
                   (->BloodPressure 130 100)]))

;; Rules are data
(pprint senior-bp-meds)

(pprint recommended-meds)

;; Rules can be created from a data structure, useful for creating
;; higher-level declarative DSLs.
(let [rule-structure {:lhs [{:type 'strangeloop.demo.Demographics
                             :constraints '[(= ?age (age birth-date))
                                            (> (age birth-date) 60)]}]}

      session (mk-session [rule-structure])]

  (pprint (-> session
              (insert (->Demographics "Alice"
                                      (-> 70 t/years t/ago)))
              (fire-rules)
              (query rule-structure))))

;; And we can inspect session state as well.
(let [session (-> (mk-session)
                  (insert (->Demographics "Alice"
                                          (-> 70 t/years t/ago))
                          (->BloodPressure 130 100)
                          (->ChronicKidneyDisease))
                  (fire-rules))]

  (ui/show-session session))

;;;;;;;;;;;;;;;;;;;
;; Vizualization demo

;; The data can be leveraged
(ui/show-logic-graph '[strangeloop.demo])

)
