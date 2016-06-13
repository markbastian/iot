(ns iot.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [reagent.core :as reagent :refer [atom]]
            [ajax.core :refer [GET POST]]
            [cljs-http.client :as http]
            [cljs.core.async :refer [chan <! >!]]
            [cljs.pprint :refer [pprint]]
            ;[clojure.spec :as s]
            ))

(enable-console-print!)

;https://docs.particle.io/reference/javascript/
(def api "https://api.particle.io")
(def device-api (str api "/v1/devices"))

(def state (atom {:access_token (.getItem js/sessionStorage "token") }))

(defn get-devices [c token]
  (go (let [{:keys [status body]}
            (<! (http/get device-api
                          {:query-params {:access_token token}
                           :with-credentials? false}))]
        (>! c {:devices body}))))

(defn async-login [c username password]
  (go (let [{:keys[status body]}
          (<! (http/post (str api "/oauth/token")
                         {:form-params {:grant_type "password"
                                        :username username
                                        :password password
                                        :client_id "particle"
                                        :client_secret "particle"}
                          :with-credentials? false}))]
      (if (= 200 status)
        (>! c (select-keys body [:access_token]))
        (prn "ERROR!")))))

(defn particle-call [{:keys [deviceId] :as state} function args]
  (go (let [res (<! (http/post (str device-api "/" deviceId "/" function)
                         {:form-params (into (select-keys state [:access_token])
                                             { :arg args})
                          :with-credentials? false}))]
        (pprint res))))

(defn particle-var [c {:keys [deviceId] :as state} varname]
  (go (let [{:keys [status body]} (<! (http/get
                                       (str device-api "/" deviceId "/" varname)
                                       {:query-params state
                                        :with-credentials? false}))]
        (if (= 200 status)
          (let [{:keys[result name]} body]
            (>! c {(keyword name) result}))
          (prn "ERROR!")))))

(defn login-form [c]
  [:div.container
   [:div.form-group
    [:label "Username"]
    [:div.input-group
     [:span.input-group-addon.glyphicon.glyphicon-user]
     [:input#username.form-control
      {:type :text :placeholder "Username" :aria-describedby :basic-addon1}]]]
   
   [:div.form-group
    [:label "Password"]
    [:div.input-group
     [:span.input-group-addon.glyphicon.glyphicon-eye-open]
     [:input#password.form-control
      {:type :password :placeholder "Password" :aria-describedby :basic-addon2}]]]

   [:div.text-center
    [:button.btn-primary.btn.lg
     {:on-click #(async-login
                  c
                  (-> js/document (.getElementById "username") .-value)
                  (-> js/document (.getElementById "password") .-value))}
     "Get Access Token"]]])

(defn process-channel [c state]
  (go-loop []
    (when-let [{:keys [devices access_token] :as m} (<! c)]
      (cond
        access_token (do (swap! state into m))
        devices (swap! state (fn [s] (cond-> (into s m)
                                       (pos? (count devices))
                                       (assoc :deviceId (-> devices first :id)))))
        :default (prn m))
      (recur))))

(defn device-status [state]
  (let [{:keys [devices deviceId]} @state]
    [:div
     [:label "Device Status"]
     (if devices
       [:select#deviceid.form-control
        {:on-click #(swap! state assoc :deviceId
                     (-> js/document (.getElementById "deviceid") .-value))}
        (doall (for [{:keys [name id]} devices]
                 [:option {:key id :value id} name]))]
       [:label (with-out-str (pprint devices))])]))

(defn hello-world []
  (let [;state (atom {:access_token (.getItem js/sessionStorage "token") })
        channel (chan)]
    (process-channel channel state)
    (add-watch state :token-storage
               (fn [_ _ _ {:keys [access_token]}]
                 (when access_token (.setItem js/sessionStorage "token" access_token))))
    ;(get-devices state)
    (fn []
      [:div.container
       [:h3.text-center "Clojure IoT Controller"]
       (when-not (:access_token @state) (login-form channel))
       [:div]

       (device-status state)
       [:hr]
       [:label (str "Access token: " (:access_token @state))]
       [:label (str "deviceId: " (:deviceId @state))]
       [:hr]
       [:button.btn-primary.btn.lg
        {:on-click
         #_(fn [_] (get-devices state))
         (fn [_] (do
                   (prn (:access_token @state))
                   (get-devices channel (:access_token @state))
                   ))}
        "Refresh Devices"]
       [:div.container
        [:button.btn-primary.btn.lg
         {:on-click
          (fn [_] (particle-call @state "led" "on"))}
         "Turn LED On"]
        [:button.btn-primary.btn.lg
         {:on-click
          (fn [_] (particle-call @state "led" "off"))}
         "Turn LED Off"]
        [:button.btn-primary.btn.lg
         {:on-click
          (fn [_] (particle-var channel @state "analogValue"))}
         "Get Value"]]
       ])))

(reagent/render-component
 [hello-world]
 (. js/document (getElementById "app")))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )

;This works

