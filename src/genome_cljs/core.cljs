(ns genome-cljs.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop alt!]])
  (:require [cljs-http.client :as http]
            [cljs.core.async :refer [<! chan put! take! >! alts! timeout] :as async]
            ))

(enable-console-print!)

(def *genome-url* "https://genome.klick.com:443")
(def *genome-api-url* (str *genome-url* "/api/"))
(defn genome [url] (str *genome-api-url* url))

(defn- extract-content
  "Extract the content from a typical Genome reply"
  []
  (map #(-> % :body :Entries)))

(defn <all-active-users
  "Gets all active Genome users"
  []
  (http/jsonp (genome "User/Search")
            {:channel (chan 1 (extract-content))}))

(defn extract-userids
  "Gets all userids from a list of users"
  [users]
  (map :UserID users))

(defn <users-details
  "Gets the profile details for one or more userids"
  [userids]
  (let [userids-vec (into [] userids) ; just to be sure
        param-string (clojure.string/join "," userids-vec)
        out (chan)]
    (go
      (>! out (<! (http/jsonp (genome "User.json")
                              {:query-params {:UserIDs param-string}
                               :channel (chan 1 (extract-content))})))
      (async/close! out))
    out))

(defn add-full-picturepath
  "Genome gives only local PhotoPath urls, this fixes it"
  [user]
  (update-in user [:PhotoPath] #(str *genome-url* (clojure.string/replace %1 #" " "%20"))))

(defn filter-in?
  "Of a sequence of typical /User profile pages, filter the ones that are currently in the building"
  [grouped-users]
  (reduce-kv (fn [m k v]
               (if (and k
                          (.startsWith k "In"))
                 (assoc m k v)
                 m)) {} grouped-users))

(defn <get-all-active-userid-profiles 
  "get the profiles of all active klicksters"
  [userids]
  (let [chunked-ids (partition-all 200 userids)
        chunked-results (map <users-details chunked-ids)]
    (->> chunked-results
         (async/merge) ;put all results on a single channel
         (async/reduce concat []) ;return a channel that'll end up having all results
         )))

(defn <get-all-active-klickster-profiles
  "gets all active klickster profiles"
  []
  (let [out (chan)]
    (go
      (let [all-klicksters (<! (<all-active-users))
            all-userids (extract-userids all-klicksters)
            all-profiles (<! (<get-all-active-userid-profiles all-userids))]
        (>! out all-profiles)))
    out))

(defn <all-active-klickster-profiles-filtered 
  "get the profiles of all active klicksters and filter them by all added filter/predicate functions"
  [& pred-fns]
  (let [out (chan)]
    (go
      (>! out
          (filter (apply every-pred pred-fns) (<! (<get-all-active-klickster-profiles)))))
    out))

(defn gtest [] :quux )
