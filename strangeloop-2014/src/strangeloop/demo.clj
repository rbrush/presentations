(ns strangeloop.demo
  "Demo code for Strangeloop 2014."
  (:require [clara.rules :refer :all]
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

(s/defrecord ChronicKidneyDisease [severity :- (s/enum :mild :moderate :severe)])

(s/defrecord Referral [type :- (s/enum :nephropathy :podiatry :hypertension)
                       urgency :- (s/enum :low :medium :high)])

(s/defrecord HighRiskPatient [demographics :- Demographics
                              condition-severity :- (s/enum :mild :moderate :severe)])

;;;;;;;;;;;;;;;;;;;
;; Demo rules.

(defrule moderate-hypertensive
  "Detect moderate hypertension."
  [BloodPressure (> 140 systolic 120)]
  =>
  (insert! (->Hypertensive :moderate)))

(defrule severe-hypertensive
  "Detect severe hypertension."
  [BloodPressure (>= systolic 140)]
  =>
  (insert! (->Hypertensive :severe)))

(defrule senior-bp-meds
  "Recommend appropriate medication for hypertensive patients."
  [Hypertensive (or (= :severe severity)
                    (= :moderate severity))]
  [Demographics (> (age birth-date) 60)]
  =>
  (insert! (->RecommendedMeds :lisinopril)))

(defrule bp-with-chronic-kidney
  "Hypertensive with chronic kidney should be referred
   to a nephrologist."
  [Hypertensive (or (= :severe severity)
                    (= :moderate severity))]
  [ChronicKidneyDisease]
  =>
  (insert! (->Referral :nephropathy :medium)))

(defrule high-hypertensive-referral
  "Hypertensive with high severity should be referred to follow up."
  [Hypertensive (= :severe severity)]
  =>
  (insert! (->Referral :hypertension :high)))

(defrule high-risk-patient
  "Hypertensive with chronic kidney should be actively managed."
  [ChronicKidneyDisease (= ?severity severity)]
  [Referral (= :high urgency)]
  [?demog <- Demographics]
  =>
  (insert! (->HighRiskPatient ?demog ?severity)))

(defquery recommended-meds
  "Recommend meds query."
  []
  [RecommendedMeds (= ?type type)])

(defquery recommended-referrals
  "Returns recommended referrals."
  []
  [Referral (= ?type type)])

(defquery risk-by-severity
  "Query to find high-risk patients with the given condition severity."
  [:?severity]
  [?patient <- HighRiskPatient (= ?severity condition-severity)])

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

;; And retraction is simple because "equals" actually means something.
(pprint (query (-> s2
                   (retract (->BloodPressure 130 100))
                   (fire-rules))
               recommended-meds))

;; A macro for easily defining an empty, reusable session.
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

;; We can generate these from a higher-level DSL.
(pprint recommended-meds)

;; And we can inspect session state as well.
(def inspected-session
  (-> (mk-session :cache false)
      (insert (->Demographics "Alice"
                              (-> 70 t/years t/ago))
              (->BloodPressure 150 120)
              (->ChronicKidneyDisease :severe))
      (fire-rules)))

;; From the console...
(inspect/explain-activations inspected-session)

;; ...and visually
(ui/show-session inspected-session)

;;;;;;;;;;;;;;;;;;;
;; Vizualization demo

;; The data can be leveraged
(ui/show-logic-graph '[strangeloop.demo])

)
