# a push-pull system
# events are pushed to queues
# then things can pull from the queues

(import ./extra_channel :prefix "")

# first some example queues
# we use ev/chan just because it's a queue
# we won't use the event handling of ev
(def mouse (ev/chan 10)) # mouse events, such as `[:down  [30  100]]`
#                                                  ^ kind  ^ x ^ y

(def tick (ev/chan 1))         # delta times, eg `10` (ms)
(def render-q (ev/chan 1))     # render calls, will just call the render function
#                              # when pulled. you can put any value here
(def callbacks @{:changed false}) # table callbacks, eg @{:down |(print "hello")}

(defn push-callback!
  [ev cb]
  (-> callbacks
      (update ev (fn [chan]
                   (default chan (ev/chan 1))
                   (ev/push! chan cb)
                   chan))
      (put :changed true)))


# then we want to be able to pull
# multiple things should be able to pull from it
# essentially splitting the value

(defn pull
  [pullable pullers]
  (when-let [v (case (type pullable)
                 :core/channel (ev/pop pullable)
                 :table        (when (pullable :changed)
                                 (put pullable :changed false))
                 (error (string (type pullable) " is not a pullable.")))]
    (loop [puller :in pullers]
      (case (type puller)
        :function     (puller v)
        :core/channel (ev/push! puller v)
        :table        (:pull puller v)
        (error (string "Pulling not implemented for " (type puller)))))
    v)         # if there was a value, we return it
  )

(defn pull-all
  [pullable pullers]
  (while
    (pull pullable pullers)
    nil))

(print)
(ev/vs mouse)

(defn put!
  [state k v]
  (-> state
      (put k v)
      (put :changed true)))

(defn update!
  [state f & args]
  (-> state
      (update f ;args)
      (put :changed true)))

(defn handle-callbacks
  [callbacks]
  (loop [[ev cbs] :pairs callbacks
         :when (not= ev :changed)]
    (pull-all cbs [apply]))
  
  (loop [k :in (keys callbacks)]
    (put callbacks k nil)))

(defn record-all
  [pullables]
  (loop [[pullable pullers] :pairs pullables]
    (case (type pullable)
      :core/channel (array/push pullers @{:history (ev/chan 10000)
                                          :pull (fn [self ev] (update self :history ev/push! ev))})
      :table        (array/push pullers @{:history (table/clone pullable)
                                          :pull (fn [self ev] nil)})))
  
  pullables)

(defn fresh?
  [pullable]
  (case (type pullable)
    :core/channel (pos? (ev/count pullable))
    :table        (pullable :changed)))



