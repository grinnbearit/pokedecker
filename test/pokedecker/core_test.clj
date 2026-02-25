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

(deftest build-expansion-code-index-test
  (is (= {"base set" "BS"
          "diamond and pearl" "DP"}
         (sut/build-expansion-code-index
          [{:name "Base Set" :code "BS"}
           {:name "Diamond & Pearl" :code "DP"}]))))

(deftest expansion-name->code-cache-test
  (sut/clear-expansion-code-cache!)
  (let [calls (atom 0)]
    (with-redefs [sut/!scrape-expansions (fn []
                                           (swap! calls inc)
                                           [{:name "Base Set" :code "BS"}
                                            {:name "WOTC Promos" :code "WP"}])]
      (is (= "BS" (sut/!expansion-name->code "Base Set")))
      (is (= "BS" (sut/!expansion-name->code " base   set ")))
      (is (= "WP" (sut/!expansion-name->code "WOTC Promos")))
      (is (nil? (sut/!expansion-name->code "Not A Set")))
      (is (= 1 @calls))))
  (sut/clear-expansion-code-cache!))

(deftest parse-card-row-test
  (let [html "<table><tr data-hover=\"...\">
                <td><span class=\"card-set\">WP</span></td>
                <td><a href=\"/cards/WP/1\">1</a></td>
                <td><a href=\"/cards/WP/1\">Pikachu</a></td>
                <td class=\"md-only\">
                  <a href=\"/cards/WP/1\"><span class=\"ptcg-symbol\">L</span> Basic</a>
                </td>
                <td class=\"md-only\"><a href=\"/cards/WP/1\"></a></td>
              </tr></table>"
        tr (.selectFirst (Jsoup/parse html) "tr")]
    (is (= {:number "1"
            :name "Pikachu"
            :type "Basic"
            :rarity nil}
           (sut/parse-card-row tr)))))

(deftest parse-card-row-with-rarity-test
  (let [html "<table><tr data-hover=\"...\">
                <td><span class=\"card-set\">BS</span></td>
                <td><a href=\"/cards/BS/1\">1</a></td>
                <td><a href=\"/cards/BS/1\">Alakazam</a></td>
                <td class=\"md-only\">
                  <a href=\"/cards/BS/1\"><span class=\"ptcg-symbol\">P</span> Stage 2</a>
                </td>
                <td class=\"md-only\"><a href=\"/cards/BS/1\">Holo Rare</a></td>
              </tr></table>"
        tr (.selectFirst (Jsoup/parse html) "tr")]
    (is (= {:number "1"
            :name "Alakazam"
            :type "Stage 2"
            :rarity "Holo Rare"}
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

(deftest parse-bulbapedia-card-link-test
  (is (= {:expansion "Base Set"
          :card-number "6"}
         (sut/parse-bulbapedia-card-link "/wiki/Gyarados_(Base_Set_6)")))
  (is (= {:expansion "Diamond & Pearl"
          :card-number "123"}
         (sut/parse-bulbapedia-card-link "/wiki/Foo_(Diamond_%26_Pearl_123)"))))

(deftest bulbapedia-deck-url-test
  (is (= "https://bulbapedia.bulbagarden.net/wiki/Overgrowth_(TCG)"
         (#'sut/bulbapedia-deck-url "Overgrowth")))
  (is (= "https://bulbapedia.bulbagarden.net/wiki/Brushfire_(TCG)"
         (#'sut/bulbapedia-deck-url "Brushfire"))))

(deftest parse-bulbapedia-deck-row-test
  (let [html "<table class=\"roundy\"><tr>
                <td style=\"text-align:center;\">2×</td>
                <td><a href=\"/wiki/Magikarp_(Base_Set_35)\" title=\"Magikarp (Base Set 35)\">Magikarp</a></td>
                <th>Type</th>
                <td>Uncommon</td>
              </tr></table>"
        tr (.selectFirst (Jsoup/parse html) "tr")]
    (is (= {:count 2
            :name "Magikarp"
            :expansion "Base Set"
            :card-number "35"}
           (sut/parse-bulbapedia-deck-row tr)))))

(deftest parse-bulbapedia-deck-doc-test
  (let [html "<html><body>
                <table class=\"roundy\">
                  <tr><th>Quantity</th><th>Card</th><th>Type</th><th>Rarity</th></tr>
                  <tr>
                    <td>1×</td>
                    <td><a href=\"/wiki/Gyarados_(Base_Set_6)\">Gyarados</a></td>
                    <th>Water</th>
                    <td>Rare Holo</td>
                  </tr>
                  <tr>
                    <td>3×</td>
                    <td><a href=\"/wiki/Potion_(Base_Set_94)\">Potion</a></td>
                    <th>Trainer</th>
                    <td>Common</td>
                  </tr>
                </table>
                <table class=\"roundy\">
                  <tr><th>Something else</th></tr>
                  <tr><td>Ignore me</td></tr>
                </table>
              </body></html>"
        doc (Jsoup/parse html)]
    (is (= [{:count 1 :name "Gyarados" :expansion "Base Set" :card-number "6"}
            {:count 3 :name "Potion" :expansion "Base Set" :card-number "94"}]
           (vec (sut/parse-bulbapedia-deck-doc doc))))))
