(ns pokedecker.core-test
  (:require [clojure.test :refer :all]
            [pokedecker.core :as sut])
  (:import (org.jsoup Jsoup)))

(deftest parse-limitless-date-test
  (is (= "2026-01-30"
         (some-> (sut/parse-limitless-date "30 Jan 26") str)))
  (is (= "1999-01-01"
         (some-> (sut/parse-limitless-date "1 Jan 99") str))))

(deftest parse-expansion-row-test
  (let [html "<table><tr>
                <td><a href=\"/cards/ASC\"><img class=\"set\" alt=\"ASC\"> Ascended Heroes <span class=\"code annotation\">ASC</span></a></td>
                <td><a href=\"/cards/ASC\">30 Jan 26</a></td>
                <td><a href=\"/cards/ASC\">295</a></td>
              </tr></table>"
        tr (.selectFirst (Jsoup/parse html) "tr")]
    (is (= {:name "Ascended Heroes"
            :code "ASC"
            :release-date "2026-01-30"}
           (sut/parse-expansion-row tr)))))

(deftest parse-expansion-row-without-date-test
  (let [html "<table><tr>
                <td><a href=\"/cards/WP\"> WOTC Promos <span class=\"code annotation\">WP</span></a></td>
                <td><a href=\"/cards/WP\"></a></td>
              </tr></table>"
        tr (.selectFirst (Jsoup/parse html) "tr")]
    (is (= {:name "WOTC Promos"
            :code "WP"
            :release-date nil}
           (sut/parse-expansion-row tr)))))

(deftest parse-card-row-test
  (let [html "<table><tr data-hover=\"...\">
                <td><span class=\"card-set\">WP</span></td>
                <td><a href=\"/cards/WP/1\">1</a></td>
                <td><a href=\"/cards/WP/1\">Pikachu</a></td>
                <td class=\"md-only\">
                  <a href=\"/cards/WP/1\"><span class=\"ptcg-symbol\">L</span> Basic</a>
                </td>
              </tr></table>"
        tr (.selectFirst (Jsoup/parse html) "tr")]
    (is (= {:number "1"
            :name "Pikachu"
            :type "Basic"}
           (sut/parse-card-row tr)))))

(deftest parse-card-image-url-test
  (let [html "<html><body>
                <div class=\"card-image\">
                  <img class=\"card\" src=\"https://images.pokemontcg.io/basep/1.png\"
                       data-src=\"https://images.pokemontcg.io/basep/1_hires.png\">
                </div>
              </body></html>"
        doc (Jsoup/parse html)]
    (is (= "https://images.pokemontcg.io/basep/1_hires.png"
           (sut/parse-card-image-url doc)))))
