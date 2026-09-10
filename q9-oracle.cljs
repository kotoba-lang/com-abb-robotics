(ns q9-oracle
  "The Clojure side of the whole-component acceptance: run `abb_robotics.main`
   -- the oracle this port is a port OF -- under nbb, and print the same
   numbers the aarch64 artifact prints.

     nbb --classpath src q9-oracle.cljs

   `migration-record.edn` recorded this repository's whole-component build as
   :partial-and-running with `:not-claimed` naming exactly this gap: the
   oracle-* functions existed and were RUN, and their values discriminated, but
   they had never been compared value-by-value against main.cljc. Running a
   thing and agreeing with the thing it replaces are two different claims, and
   only the second one is a port.

   ## Two kinds of answer, and they are not equally strong

   * The i64 answers -- route count, as-int, as-bool, page-limit, store counts,
     filter counts, pagination counts, handler status codes -- call
     `abb_robotics.main` and count. Nothing is rendered on the way, so a
     disagreement is a disagreement about the component's BEHAVIOUR.

   * The hash answers go through a rendering. `str-hash` is the same rolling
     hash the .kotoba computes (acc*131 + code point, mod 1000000007, seeded 7;
     every intermediate stays under 2^53, so a JS double holds it exactly). But
     the STRING being hashed is built here, from main.cljc's values, in this
     port's JSON convention. A disagreement therefore means EITHER a behaviour
     difference OR a rendering difference, and has to be read before it is
     believed. They are printed in a separate section for that reason.

   ## The control

   `abb_robotics.main` declares `:coerce {:payloadKg :float}` on Robot, and the
   component's float arm preserves the raw token instead of coercing --
   recorded in migration-record.edn as :blocked, an implementation gap rather
   than something to design around. So the float layer MUST disagree. A run of
   this harness in which everything matches has not demonstrated that it can
   detect a mismatch, and should not be read as a pass."
  (:require [abb_robotics.main :as m]
            [clojure.string :as s]))

(defn str-hash [x]
  (reduce (fn [acc ch] (mod (+ (* acc 131) (.charCodeAt ch 0)) 1000000007)) 7 (seq (str x))))

;; --- fixtures: the same documents the .kotoba carries ----------------------

(def fixture-text
  {0 "12" 1 " 42 " 2 "-7" 3 "12abc" 4 "" 5 "true" 6 "TRUE" 7 "Yes" 8 "on"
   9 "1" 10 "0" 11 "false" 12 "null" 13 "nope"})

(def fixture-data
  {0 {:partNumber "P-1" :name "Bracket"}
   1 {:partNumber "P-1"}
   2 {:partNumber "P-1" :name "Bracket" :bogus "x"}
   3 {:model "IRB-6700" :status "idle" :payloadKg "150.5"}
   4 {:quantity "3"}
   5 {}})

(defn spec-for [e] (first (filter #(= (:entity %) e) m/entity-specs)))

(defn seeded-row [i]
  {:id (str "abbrobot_par_" i)
   :revision i
   :partNumber (str "P-" i)
   :name (if (even? i) "Bracket" "Flange")})

(defn seeded-store [n]
  (let [st (m/fresh-store)]
    (doseq [i (range n)] (m/persist! st "Part" (seeded-row i)))
    st))

;; A BOM row pointing at one Part through BOTH ref fields. This is the pilot's
;; novelty: every spec in the sibling ports carried exactly one ref, so
;; `expand`'s fold ran over a single pair, and a fold over one is not a fold.
(defn cred-row [i]
  {:id (str "abbrobot_bom_" i)
   :parentPartId (str "abbrobot_par_" i)
   :childPartId (str "abbrobot_par_" i)
   :quantity i})

(defn seeded-both [n]
  (let [st (seeded-store n)] (m/persist! st "BOM" (cred-row 1)) st))

(defn row [label v] (println (str label "\t" v)))

;; --- i64 answers: no rendering between main.cljc and the number -----------

(println "; oracle = abb_robotics.main under nbb")
(row "main(route-count)" (count m/routes))
(row "entity-count" (count m/entity-specs))

(println "; as-int")
(doseq [i [0 1 2 3 4 12]] (row (str "as-int " i) (m/as-int (fixture-text i))))

(println "; as-bool")
(doseq [i [5 6 7 8 9 10 11 4 13]]
  (row (str "as-bool " i) (if (m/as-bool (fixture-text i)) 1 0)))

(println "; page-limit")
(doseq [r [-5 0 1 20 99 100 101 250]] (row (str "page-limit " r) (m/page-limit r)))

(println "; store")
(row "store 0" (count (m/query (seeded-store 3) "Part")))
(row "store 1" (let [st (seeded-store 3)] (m/retract! st "Part" "abbrobot_par_1")
                 (count (m/query st "Part"))))
(row "store 2" (let [st (seeded-store 3)] (m/retract! st "Part" "nope")
                 (count (m/query st "Part"))))
(row "store 3" (count (m/query (seeded-both 3) "BOM")))

(println "; filters")
(let [rows (m/query (seeded-store 5) "Part")
      fields (:fields (spec-for "Part"))]
  (row "filters 0" (count (m/apply-filters rows {} fields)))
  (row "filters 1" (count (m/apply-filters rows {:name "Bracket"} fields)))
  (row "filters 2" (count (m/apply-filters rows {:name ""} fields)))
  (row "filters 3" (count (m/apply-filters rows {:partNumber "P-3"} fields)))
  (row "filters 4" (count (m/apply-filters rows {:partNumber "P-99"} fields)))
  (row "filters 5" (count (m/apply-filters rows {:notAField "x"} fields))))

(println "; paginate  page*10 + has_more")
(doseq [[n lim] [[5 2] [5 5] [5 0] [5 200] [3 2] [0 2]]]
  (let [rows (m/query (seeded-store n) "Part")
        [page more] (m/paginate rows {:limit lim})]
    (row (str "paginate " n " " lim) (+ (* 10 (count page)) (if more 1 0)))))

(println "; handlers (status)")
(let [mk (fn [] (seeded-store 2))]
  (row "handlers 0" (second (m/handle-create (mk) "Part" (fixture-data 0))))
  (row "handlers 1" (second (m/handle-create (mk) "Part" (fixture-data 1))))
  (row "handlers 2" (second (m/handle-create (mk) "Part" (fixture-data 2))))
  (row "handlers 3" (second (m/handle-list (mk) "Part" {})))
  (row "handlers 4" (second (m/handle-get (mk) "Part" "abbrobot_par_0" {})))
  (row "handlers 5" (second (m/handle-get (mk) "Part" "nope" {})))
  (row "handlers 6" (second (m/handle-delete (mk) "Part" "abbrobot_par_0")))
  (row "handlers 7" (second (m/handle-delete (mk) "Part" "nope"))))

;; --- the two-ref expand, as COUNTS rather than hashes ----------------------
;;
;; The .kotoba hashes the rendered record. Rendering is this port's convention,
;; so a hash comparison here would confound behaviour with layout. Counting the
;; expanded keys instead asks the question the fold is actually for: did it run
;; once, twice, or not at all.

(defn expanded-keys [rec params refs]
  (count (filter #(s/ends-with? (name %) "_obj")
                 (keys (m/expand (seeded-both 3) rec params refs)))))

(println "; expand  (count of _obj keys the fold added)")
(let [rec (cred-row 1)
      bom (:refs (spec-for "BOM"))
      part (:refs (spec-for "Part"))]
  (row "expand parent-only"  (expanded-keys rec {:expand "parentPartId"} bom))
  (row "expand unknown"      (expanded-keys rec {:expand "other"} bom))
  (row "expand missing-row"  (expanded-keys (cred-row 9) {:expand "parentPartId"} bom))
  (row "expand no-refs-spec" (expanded-keys rec {:expand "parentPartId"} part))
  (row "expand BOTH"         (expanded-keys rec {:expand "parentPartId,childPartId"} bom)))

;; --- the control: the float arm MUST disagree ------------------------------

(println "; float  (the arm recorded as :blocked -- these MUST differ from the artifact)")
(doseq [[i v] [[0 "12.5"] [1 "\"12.5\""] [2 "\"abc\""] [3 "0"] [4 ""]]]
  (row (str "coerce-float " i) (str-hash (m/coerce-field :float v))))

(println "; int/bool coercion (should AGREE)")
(doseq [[i k v] [[0 :int " 42 "] [1 :int "7"] [2 :bool "YES"] [3 :bool "false"] [4 nil "abc"]]]
  (row (str "coerce " i) (str-hash (m/coerce-field k v))))
