(ns damai.core
  (:gen-class)
  (:import [java.nio ByteBuffer ByteOrder]
           [java.io File FileInputStream])
  (:use [clojure.string :only (split)])
  (:require [clojure.pprint :as pp]
            [clj-http.client :as client]))


(defmacro ubyte
  [b]
  `(bit-and ~b 0xff))

(defn getSeatByteArray
  [url]
  (:body (client/get url {:as :byte-array :conn-timeout 1000})))

(defn getPerformID
  [content]
  (if-let [pid (re-find #"performID=(\d+)" content)]
    (second pid)))

(defn getSuffix
  [content]
  (if-let [suffix (re-find #"&suffix=([^&]+)&" content)]
    (second suffix)))

(defn getCid
  [content]
  (if-let [cid (re-find #"&cid=([^&]+)&" content)]
    (second cid)))

(defn getP3
  [content]
  (if-let [p3 (re-find #"p3=([^&]+)&" content)]
    (second p3)))

(defn getAreaList
  [content]
  (apply hash-map (mapcat #(split % #",") (split content #"\|"))))


(defn debug
  [a]
  (do (print a "\n") a))

(defn getSeatObjects
	"convert seat bytes to a vector of seatsinfo(map)"
	[^ByteBuffer bytes]
	(let [base-id (.getInt bytes)
              count (.getShort bytes)
              result (transient [])]
          (loop [pai-id 1
                 col-id 0
                 row-result (transient [])]
            (if (.hasRemaining bytes)
              (let [cls (.get bytes)]
                (cond
                 (and (bit-test cls 6) (bit-test cls 7)) (let [row-info (ubyte (.get bytes))
                                                               seat-info (transient {})
                                                               new-col-id (inc col-id)]
                                                           (-> seat-info
                                                               (conj! [:paiId pai-id])
                                                               (conj! [:rowNo (bit-shift-right row-info 1)])
                                                               (conj! [:seatId (+ base-id (if (bit-test row-info 0) (.getInt bytes) (.getShort bytes)))]))
                                                           (let [price-info (ubyte (.get bytes))]
                                                             (-> seat-info
                                                                 (conj! [:chineseBoolean (bit-test price-info 7)])
                                                                 (conj! [:priceLv (bit-and price-info 0x7f)])
                                                                 (conj! [:colId col-id]))
                                                             (let [seatno-info (ubyte (.get bytes))]
                                                               (-> seat-info
                                                                   (conj! [:seatNo (bit-shift-right seatno-info 1)]))
                                                               (let [floor-info (ubyte (.get bytes))]
                                                                 (if (>= floor-info 0xff) (print "Overflow?\n"))
                                                                 (conj! seat-info [:floorName floor-info]) ;;TODO:check >=0xFF
                                                                 (if (bit-test seatno-info 0) (-> seat-info (conj! [:familyTicket (ubyte (.get bytes))]) (conj! [:familyTicketId (.getInt bytes)])))
                                                                 (conj! row-result (persistent! seat-info))
                                                                 (recur pai-id new-col-id row-result)))))
                 (bit-test cls 7) (let [null-count (if (bit-test cls 5) (ubyte (.get bytes)) (bit-and cls 0x1f))]
                                    (dotimes [i null-count] (conj! row-result nil))
                                    (recur pai-id (inc col-id) row-result))
                 (bit-test cls 6) (do (conj! result (persistent! row-result)) (recur (inc pai-id) col-id (transient [])))
                 :else (recur pai-id col-id row-result)))
              (do (conj! result (persistent! row-result)) (persistent! result))))))

(defn pretty-print-seatsinfo
  [seats]
  (doseq [row seats]
    (doseq [seat row]
      (cond
       (map? seat) (print "O")
       (nil? seat) (print "X")))
    (print \newline)
))

(defn -main
  [& args]
  (let [main-page       (client/get (first args))
        pid             (debug (getPerformID (:body main-page)))
        xuanzuo-page    (client/get (str "http://seat.damai.cn/xuanzuo/" pid))
        suffix          (debug (getSuffix (:body xuanzuo-page)))
        cid             (debug (getCid (:body xuanzuo-page)))
        p3              (debug (getP3 (:body xuanzuo-page)))
        standdata-page  (client/get "http://flashxml.damai.cn/StandData.aspx" {:query-params {"t" "8", "cid" cid, "p3" p3}})
        area-list       (getAreaList (:body standdata-page))]
    (map (fn [e]
            (let [url (format "http://sseat.damai.cn/xuanzuo/io/%s/%s/%s/%s.txt" cid pid suffix (first e))
                  bytes-buff (.order (ByteBuffer/wrap (getSeatByteArray url)) ByteOrder/LITTLE_ENDIAN)]
              (print "Requested: " url "\n")
              (pretty-print-seatsinfo (getSeatObjects bytes-buff))))
          (filter #(not= (second %) "0") area-list))))
