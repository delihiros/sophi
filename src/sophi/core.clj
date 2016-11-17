(ns sophi.core
  (:require [net.cgrand.enlive-html]))

(defn sfmap [s f]
  (map f s))

(defn sffilter [s f]
  (filter f s))

(defn lookup-wikipedia-page [query]
  (try
    (clojure.java.io/reader (str "https://ja.wikipedia.org/wiki/" query))
    (catch Exception e nil)))

(defn extract-infobox [raw-data]
  (net.cgrand.enlive-html/select raw-data [:table.infobox]))

(defn extract-refs [td]
  (some-> td
          :content
          (sfmap #(-> % :content first))
          (sffilter identity)))

(defn extract-influences [infobox]
  (some-> infobox
          (net.cgrand.enlive-html/select [:li])
          first
          extract-refs))

(defn extract-influenced [infobox]
  (some-> infobox
          (net.cgrand.enlive-html/select [:li])
          second
          extract-refs))

(defn table->map [table]
  (apply merge
         (map
           (fn [tr] (zipmap (some-> tr
                                    (net.cgrand.enlive-html/select [:th])
                                    first :content)
                            (some-> tr
                                    (net.cgrand.enlive-html/select [:td])
                                    (sfmap extract-refs))))
           (net.cgrand.enlive-html/select table [:tr]))))

(defn get-philosopher' [philosopher]
  (if-let [infobox
           (some-> philosopher
                   lookup-wikipedia-page
                   net.cgrand.enlive-html/html-resource
                   extract-infobox)]
    (let [info-map (table->map infobox)]
      {:name philosopher
       :era (get info-map "時代")
       :region (get info-map "地域")
       :school (get info-map "学派")
       :main-interests (get info-map "研究分野")
       :ideas (get info-map "主な概念")
       :influences (extract-influences infobox)
       :influenced (extract-influenced infobox)})))

(def get-philosopher
  (memoize get-philosopher'))

(defn -main [& args]
  (loop [candidates [(get-philosopher "アリストテレス")]
         philosophers #{}]
    (println (:name (peek candidates)))
    (if (seq candidates)
      (if (contains? philosophers (-> candidates peek))
        (recur (rest candidates) philosophers)
        (recur (apply conj (rest candidates)
                      (pmap get-philosopher
                            (concat
                              (-> candidates peek :influences)
                              (-> candidates peek :influenced))))
               (conj philosophers (peek candidates))))
      (spit "./philosophers.txt" (pr-str philosophers)))))
