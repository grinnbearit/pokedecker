(ns pokedecker.core-test
  (:require [clojure.test :refer :all]
            [pokedecker.core :as sut])
  (:import (java.awt.image BufferedImage)
           (java.nio.file Files)
           (javax.imageio ImageIO)
           (org.jsoup Jsoup)))

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

(defn- write-test-png! [path]
  (let [img (BufferedImage. 2 3 BufferedImage/TYPE_INT_ARGB)]
    (.setRGB img 0 0 (unchecked-int 0xFFFF0000))
    (.mkdirs (.getParentFile (java.io.File. path)))
    (ImageIO/write img "png" (java.io.File. path))
    path))

(deftest card-image-cache-path-test
  (binding [sut/*card-image-cache-root* "tmp/test-images"]
    (is (= "tmp/test-images/WP/1.png"
           (sut/card-image-cache-path "wp" 1)))))

(deftest card-image-cache-miss-persists-and-returns-image-test
  (let [tmp-dir (.toFile (Files/createTempDirectory "pokedecker-img-cache" (make-array java.nio.file.attribute.FileAttribute 0)))
        source-path (str (.getAbsolutePath tmp-dir) "/source.png")
        source-url (.toString (.toURI (java.io.File. (write-test-png! source-path))))
        fetch-calls (atom 0)]
    (binding [sut/*card-image-cache-root* (.getAbsolutePath tmp-dir)]
      (with-redefs [sut/!fetch-card-image-url (fn [_ _]
                                                (swap! fetch-calls inc)
                                                source-url)]
        (let [img (sut/!card-image! "WP" 1)
              cached-path (sut/card-image-cache-path "WP" 1)]
          (is (instance? BufferedImage img))
          (is (= 2 (.getWidth img)))
          (is (.exists (java.io.File. cached-path)))
          (is (= 1 @fetch-calls)))))))

(deftest card-image-cache-hit-avoids-fetch-test
  (let [tmp-dir (.toFile (Files/createTempDirectory "pokedecker-img-hit" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (binding [sut/*card-image-cache-root* (.getAbsolutePath tmp-dir)]
      (let [cached-path (sut/card-image-cache-path "BS" 6)]
        (write-test-png! cached-path)
        (with-redefs [sut/!fetch-card-image-url (fn [& _]
                                                  (throw (ex-info "should not fetch" {})))]
          (let [img (sut/!card-image! "BS" 6)]
            (is (instance? BufferedImage img))
            (is (= 3 (.getHeight img)))))))))

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

(deftest enrich-deck-cards-with-limitless-codes-test
  (let [deck-cards [{:count 1 :name "Gyarados" :expansion "Base Set" :card-number "6"}
                    {:count 2 :name "Pikachu" :expansion "WOTC Promos" :card-number "1"}
                    {:count 4 :name "Foo" :expansion "Unknown Set" :card-number "9"}]
        lookup {"Base Set" "BS"
                "WOTC Promos" "WP"}]
    (is (= [{:count 1 :name "Gyarados" :expansion "Base Set" :card-number "6" :code "BS"}
            {:count 2 :name "Pikachu" :expansion "WOTC Promos" :card-number "1" :code "WP"}
            {:count 4 :name "Foo" :expansion "Unknown Set" :card-number "9" :code nil}]
           (vec (sut/enrich-deck-cards-with-limitless-codes deck-cards lookup))))))

(deftest scrape-bulbapedia-deck-cards-with-codes-test
  (with-redefs [sut/!scrape-bulbapedia-deck-cards (fn [deck-title]
                                                    (is (= "Overgrowth" deck-title))
                                                    [{:count 1 :name "Gyarados" :expansion "Base Set" :card-number "6"}
                                                     {:count 2 :name "Magikarp" :expansion "Base Set" :card-number "35"}])
                sut/!expansion-name->code (fn [expansion]
                                            ({"Base Set" "BS"} expansion))]
    (is (= [{:count 1 :name "Gyarados" :expansion "Base Set" :card-number "6" :code "BS"}
            {:count 2 :name "Magikarp" :expansion "Base Set" :card-number "35" :code "BS"}]
           (vec (sut/!scrape-bulbapedia-deck-cards-with-codes "Overgrowth"))))))

(deftest deck-image-export-filename-test
  (is (= "BS_6_001_Gyarados.png"
         (sut/deck-image-export-filename
          {:code "BS" :card-number "6" :name "Gyarados"} 1)))
  (is (= "WP_24_002______-s-Pikachu.png"
         (sut/deck-image-export-filename
          {:code "WP" :card-number "24" :name "_____'s Pikachu"} 2))))

(deftest write-deck-images-test
  (let [tmp-dir (.toFile (Files/createTempDirectory "pokedecker-deck-out" (make-array java.nio.file.attribute.FileAttribute 0)))
        source-path (str (.getAbsolutePath tmp-dir) "/source.png")]
    (write-test-png! source-path)
    (with-redefs [sut/!card-image-file! (fn [code card-number]
                                          (is (= "BS" code))
                                          (is (= "6" (str card-number)))
                                          source-path)]
      (let [written (vec (sut/!write-deck-images!
                          [{:count 2 :name "Gyarados" :expansion "Base Set" :card-number "6" :code "BS"}]
                          (str (.getAbsolutePath tmp-dir) "/export")))]
        (is (= 2 (count written)))
        (is (.exists (java.io.File. (first written))))
        (is (.exists (java.io.File. (second written))))
        (is (not= (first written) (second written)))))))

(deftest write-deck-images-missing-code-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Missing Limitless code"
       (dorun
        (sut/!write-deck-images!
         [{:count 1 :name "Gyarados" :expansion "Base Set" :card-number "6" :code nil}]
         "tmp/out")))))

(deftest export-bulbapedia-deck-images-test
  (with-redefs [sut/!scrape-bulbapedia-deck-cards-with-codes (fn [deck-title]
                                                               (is (= "Overgrowth" deck-title))
                                                               [{:count 1 :name "Gyarados" :expansion "Base Set" :card-number "6" :code "BS"}])
                sut/!write-deck-images! (fn [deck output-dir]
                                          (is (= [{:count 1 :name "Gyarados" :expansion "Base Set" :card-number "6" :code "BS"}]
                                                 (vec deck)))
                                          (is (= "output/overgrowth" output-dir))
                                          ["output/overgrowth/BS_6_001_Gyarados.png"])]
    (is (= ["output/overgrowth/BS_6_001_Gyarados.png"]
           (vec (sut/!export-bulbapedia-deck-images! "Overgrowth" "output/overgrowth"))))))
