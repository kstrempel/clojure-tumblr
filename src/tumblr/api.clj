(ns tumblr.api
  (:use [clojure.data.json :only [read-str]])
  (:require [clojure.string :as string]
            [oauth.client :as oauth]
            [clj-http.client :as http]))

(def ^:dynamic *oauth-consumer* "mWVfcnCl5foAnADkGn8qpp6hgrKLZdGbLvZyPVXbBUX8hzMlJU")
(def ^:dynamic *oauth-consumer-token-secret* "0kc9ZI7WtMn957raxbA0MjcdmRMrSbfmI5t3cKqTm09k4G5CFY")

(def consumer (oauth/make-consumer *oauth-consumer*
                                   *oauth-consumer-token-secret*
                                   "http://www.tumblr.com/oauth/request_token"
                                   "http://www.tumblr.com/oauth/access_token"
                                   "http://www.tumblr.com/oauth/authorize"
                                   :hmac-sha1))

(defmacro def-tumblr-method
  [method-name req-method req-url required-params optional-params handler]
  (let [required-fn-params (vec (map #(symbol (name %)) required-params))
        optional-fn-params (vec (map #(symbol (name %)) optional-params))]
  `(defn ~method-name
     [~@required-fn-params & rest#]
     (let [params# (apply hash-map (interleave ~required-params ~required-fn-params))

           optional-params# (apply hash-map rest#)

           req-url# (reduce (fn [url# x#] (string/replace url# (str x#) (get params# x#)))
                            ~req-url
                             (keys params#))

           req-uri# (str "https"
                         "://"
                         req-url#
                         (if (= ~req-method :get-with-api-key)
                           (str "?api_key=" *oauth-consumer*)
                           "")
                         (apply str (map (fn [param#] 
                                           (str "&" (name param#) 
                                                "=" (get optional-params# param#)))
                                         (keys optional-params#))))

           answer# (http/get req-uri#)

           tumblr-status# (:status answer#)

           tumblr-info# (if (= 200 tumblr-status#)
                          (read-str (:body answer#) :key-fn keyword)
                          {})]
       (:response tumblr-info#)))))

(def-tumblr-method info
  :get-with-api-key
  "api.tumblr.com/v2/blog/:base-hostname/info"
  [:base-hostname]
  []
  (nil))


(def-tumblr-method likes
  :get-with-api-key
  "api.tumblr.com/v2/blog/:base-hostname/likes"
  [:base-hostname]
  []
  (nil))

(def-tumblr-method posts
  :get-with-api-key
  "api.tumblr.com/v2/blog/:base-hostname/posts"
  [:base-hostname]
  [:type :id :tag :limit :offset :reblog_info :notes_info :filter]
  (nil))













