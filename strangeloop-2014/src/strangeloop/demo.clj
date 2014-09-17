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
    [person-id :- s/Int
     systolic :- s/Int
     diastolic :- s/Int])

(s/defrecord Hypertensive
    [person-id :- s/Int
     severity :- (s/enum :mild :moderate :severe)])

(s/defrecord RecommendedMeds
    [person-id :- s/Int
     type :- (s/enum :lisinopril)])

(s/defrecord Demographics
    [person-id :- s/Int
     name :- s/Str
     birth-date :- LocalDate])

(s/defrecord ChronicKidneyDisease [person-id :- s/Int
                                   severity :- (s/enum :mild :moderate :severe)])

(s/defrecord Referral [person-id :- s/Int
                       type :- (s/enum :nephropathy :podiatry :hypertension)
                       urgency :- (s/enum :low :medium :high)])

(s/defrecord HighRiskPatient [person-id :- s/Int
                              demographics :- Demographics
                              condition-severity :- (s/enum :mild :moderate :severe)])

;;;;;;;;;;;;;;;;;;;
;; Demo rules.

(defrule moderate-hypertensive
  "Detect moderate hypertension."
  [BloodPressure (= ?id person-id) (> 140 systolic 120)]
  =>
  (insert! (->Hypertensive ?id :moderate)))

(defrule severe-hypertensive
  "Detect severe hypertension."
  [BloodPressure (= ?id person-id) (>= systolic 140)]
  =>
  (insert! (->Hypertensive ?id :severe)))

(defrule senior-bp-meds
  "Recommend appropriate medication for hypertensive patients."
  [Hypertensive (= ?id person-id)
                (or (= :severe severity)
                    (= :moderate severity))]
  [Demographics (= ?id person-id)
                (> (age birth-date) 60)]
  =>
  (insert! (->RecommendedMeds ?id :lisinopril)))

(defrule bp-with-chronic-kidney
  "Hypertensive with chronic kidney should be referred
   to a nephrologist."
  [Hypertensive (= ?id person-id)
                (or (= :severe severity)
                    (= :moderate severity))]

  [ChronicKidneyDisease (= ?id person-id)]
  =>
  (insert! (->Referral ?id :nephropathy :medium)))

(defrule high-hypertensive-referral
  "Hypertensive with high severity should be referred to follow up."
  [Hypertensive (= ?id person-id)
                (= :severe severity)]
  =>
  (insert! (->Referral ?id :hypertension :high)))

(defrule high-risk-patient
  "Hypertensive with chronic kidney should be actively managed."
  [ChronicKidneyDisease (= ?id person-id) (= ?severity severity)]
  [Referral (= ?id person-id) (= :high urgency)]
  [?demog <- Demographics (= ?id person-id)]
  =>
  (insert! (->HighRiskPatient ?id ?demog ?severity)))

(defquery recommended-meds
  "Recommend meds query."
  []
  [RecommendedMeds (= ?type type) (= ?person-id person-id)])

(defquery recommended-referrals
  "Returns recommended referrals."
  []
  [Referral (= ?type type ) (= ?person-id person-id)])

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
            (insert (->Demographics 1
                                    "Alice"
                                    (-> 70 t/years t/ago))
                    (->BloodPressure 1 130 100))
            (fire-rules)
            (query recommended-meds)))

;; Sessions are immutable.
(def s1 (-> (mk-session)
            (insert (->Demographics 1
                                    "Alice"
                                    (-> 70 t/years t/ago)))
            (fire-rules)))

;; Nothing matches.
(pprint (query s1 recommended-meds))


;; Inserting a new fact creates a new, immutable session.
(def s2 (-> s1
            (insert (->BloodPressure 1 130 100))
            (fire-rules)))

;; Our rule now matches
(pprint (query s2 recommended-meds))

;; But the original is unchanged.
(pprint (query s1 recommended-meds))

;; And retraction is simple because "equals" actually means something.
(pprint (query (-> s2
                   (retract (->BloodPressure 1 130 100))
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

    (for [{:keys [?type ?person-id]} (query person-session recommended-meds)]

      {:medication ?type :for-person-id ?person-id})))

(pprint
 (get-medications [(->Demographics 1
                                   "Alice"
                                   (-> 70 t/years t/ago))
                   (->BloodPressure 1 130 100)]))

;; Rules are data
(pprint moderate-hypertensive)

;; We can generate these from a higher-level DSL.
(pprint senior-bp-meds)

;; And we can inspect session state as well.
(def inspected-session
  (-> (mk-session :cache false)
      (insert (->Demographics 1
                              "Alice"
                              (-> 70 t/years t/ago))
              (->BloodPressure 1 150 120)
              (->ChronicKidneyDisease 1 :severe))
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
