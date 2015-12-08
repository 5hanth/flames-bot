(ns webapp
  (:import (de.raysha.lib.telegram.bot.api TelegramBot BotAPI model.User))
  (:use org.httpkit.server))
  

(defn app [req]
      {:status  200
               :headers {"Content-Type" "text/html"}
               :body    "Visit <a href=\"http://telegram.me/Flames_bot\">telegram.me/Flames_bot</a> to find flames."})


(def flames-map {"F" "Friend \uD83D\uDE09" "L" "Love \uD83D\uDC99" "A" "Affection \uD83D\uDC8F"
                 "M" "Marriage \uD83D\uDC6A" "E" "Enemy \uD83D\uDC7F" "S" "Sister \uD83D\uDC81"})

(defn _do-flames [n fl]
      (let [do-split (fn [n fl]
			 (let [[xs ys] (split-at n fl)]
			      (flatten
			       (conj (butlast xs)
				     ys))))
	   c (count fl)]
	   (cond (= c 1)
		 (first fl)
		 (<= n c)
		 (_do-flames n (do-split n fl))
		 :else (let [n-n (let [r (rem n c)]
				      (if (= r 0) n r))
			    n-f (do-split n-n fl)]
			    (_do-flames n n-f)))))
			   
(defn find-freq [n1 n2]
      (let [lns (map clojure.string/lower-case [n1 n2])
	    sns (map #(remove (fn [c] (= \space c)) %1) lns)]
	   (map frequencies sns)))
	  
(defn flames-number [n1 n2]
      (let [[m1 m2] (find-freq n1 n2)
	    f-val (reduce + (for [[k v]
			(merge-with #(Math/abs (- % %2))
				    m1 m2 )
			:when (not= 0 v)] v))]
        (if (zero? f-val) 1 f-val)))

(defn do-flames [n1 n2]
      (_do-flames (flames-number n1 n2) (map str "FLAMES")))

(def last-update-id (atom (int 0)))
(def botToken "YOUR BOT-TOKEN HERE")
(def my-bot (TelegramBot. botToken ))

(defn update->chat-id [update]
  (-> update .getMessage .getChat (get "id")))


(defn reply-to-message [message]
  (let [user-text (or (-> message .getText) "Can't get you..")
        flames-regex #"[fF][Ll][Aa][Mm][Ee][Ss]\s+(\w+)\s+(\w+)\s*"]
    (cond (= user-text "/start")
            (str "Hi " (-> message .getChat (get "first_name")) " \uD83D\uDE0D" "\ntext me:\nflames yourname yourcrushname\n to find flames \uD83D\uDE0E")
          (re-find  flames-regex user-text) 
            (let [[_ n1 n2] (re-find flames-regex user-text)]
              (str "Flames of " n1 " and " n2 " is " (flames-map (do-flames n1 n2))))
          :else user-text)))

(defn start-bot [& args]
      (while true (doall 
                    (for [update (.getUpdates my-bot 
                                              (int @last-update-id) (int 10) (int 10))] 
                      (do (.sendMessage my-bot (int (update->chat-id update)) 
                                        (reply-to-message (-> update .getMessage)))
                          (reset! last-update-id 
                                  (-> update .getUpdate_id inc )))))))

(defn -main [& args]
    (let [port (Integer/parseInt (get (System/getenv) "OPENSHIFT_CLOJURE_HTTP_PORT" "8080"))]
          (let [ip (get (System/getenv) "OPENSHIFT_CLOJURE_HTTP_IP" "0.0.0.0")]
            (future (start-bot))
              (run-server app {:ip ip :port port}))))
