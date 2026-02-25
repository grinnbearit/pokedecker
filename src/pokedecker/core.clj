(ns pokedecker.core
  (:require [clojure.string :as str])
  (:import (java.net URLDecoder)
           (java.time LocalDate)
           (java.time.format TextStyle)
           (java.util Locale)
           (org.jsoup Jsoup)
           (org.jsoup.nodes Element)))

(def ^:private cards-url "https://limitlesstcg.com/cards")
(def ^:private bulbapedia-url "https://bulbapedia.bulbagarden.net")

(def ^:private month->number
  (into {}
        (for [m (range 1 13)]
          [(-> (java.time.Month/of m)
               (.getDisplayName TextStyle/SHORT Locale/ENGLISH)
               str/lower-case)
           m])))

(defn- normalize-2-digit-year [yy]
  (let [candidate (+ 2000 yy)
        max-future (.plusYears (LocalDate/now) 5)]
    (if (.isAfter (LocalDate/of candidate 1 1) max-future)
      (- candidate 100)
      candidate)))

(defn parse-limitless-date
  "Parses Limitless release dates like \"30 Jan 26\" into a LocalDate."
  [s]
  (when-let [date-str (some-> s str/trim not-empty)]
    (when-let [[_ d mon yy] (re-matches #"(?i)^\s*(\d{1,2})\s+([A-Za-z]{3})\s+(\d{2})\s*$" date-str)]
      (let [month (get month->number (str/lower-case mon))
            year (normalize-2-digit-year (Long/parseLong yy))]
        (when month
          (LocalDate/of year month (Long/parseLong d)))))))

(defn parse-expansion-row
  "Parses a single <tr> from the Limitless sets table into a map.
   Returns nil for rows that are headings or malformed."
  [^Element tr]
  (let [cells (.select tr "td")]
    (when (>= (.size cells) 2)
      (let [name-cell (.get cells 0)
            date-cell (.get cells 1)
            link (.selectFirst name-cell "a[href^=/cards/]")
            code-el (.selectFirst name-cell "span.code")
            code (some-> code-el .text str/trim not-empty)
            name (some-> link .ownText str/trim not-empty)
            release-date (parse-limitless-date (.text date-cell))]
        (when (and name code)
          {:name name
           :code code
           :release-date (some-> release-date str)})))))

(defn- card-list-url [expansion-code]
  (str cards-url "/" (str/trim expansion-code) "?display=list"))

(defn- card-profile-url [expansion-code card-number]
  (str cards-url "/" (str/trim expansion-code) "/" (str/trim (str card-number))))

(defn- bulbapedia-deck-url [deck-title]
  (str bulbapedia-url "/wiki/"
       (-> deck-title
           str/trim
           (str/replace " " "_"))
       "_(TCG)"))

(defn- cleaned-text
  "Returns text content after removing decorative nodes matched by selector."
  [^Element el selector]
  (when el
    (let [copy (.clone el)]
      (.remove (.select copy selector))
      (some-> (.text copy) str/trim not-empty))))

(defn parse-card-row
  "Parses a single card list <tr> into {:number :name :type :rarity}.
   Returns nil for header/malformed rows."
  [^Element tr]
  (let [cells (.select tr "td")]
    (when (>= (.size cells) 5)
      (let [number (some-> (.get cells 1) .text str/trim not-empty)
            name (some-> (.get cells 2) .text str/trim not-empty)
            type (cleaned-text (.get cells 3) ".ptcg-symbol")
            rarity (some-> (.get cells 4) .text str/trim not-empty)]
        (when (and number name)
          {:number number
           :name name
           :type type
           :rarity rarity})))))

(defn parse-card-image-url
  "Parses a card detail page and returns the card image URL.
   Prefers the high-res `data-src` attribute when present."
  [^org.jsoup.nodes.Document doc]
  (when-let [img (.selectFirst doc "div.card-image img")]
    (or (some-> (.attr img "data-src") str/trim not-empty)
        (some-> (.attr img "src") str/trim not-empty))))

(defn- url-decode [s]
  (URLDecoder/decode s "UTF-8"))

(defn parse-bulbapedia-card-link
  "Parses a Bulbapedia card link href like /wiki/Gyarados_(Base_Set_6)
   into {:expansion \"Base Set\" :card-number \"6\"}."
  [href]
  (when-let [[_ slug] (when-let [s (some-> href str/trim)]
                        (re-matches #"(?:https?://[^/]+)?/wiki/([^?#]+)" s))]
    (let [decoded (url-decode slug)]
      (when-let [[_ suffix] (re-matches #".*?\((.+)\)$" decoded)]
        (let [idx (.lastIndexOf ^String suffix "_")]
          (when (pos? idx)
            (let [expansion (subs suffix 0 idx)
                  card-number (subs suffix (inc idx))]
              (when (and (not (str/blank? expansion))
                         (not (str/blank? card-number)))
                {:expansion (str/replace expansion "_" " ")
                 :card-number (str/replace card-number "_" " ")}))))))))

(defn parse-bulbapedia-deck-row
  "Parses a Bulbapedia decklist row into {:count :name :expansion :card-number}.
   Returns nil for non-deck rows."
  [^Element tr]
  (let [cells (.select tr "td")]
    (when (>= (.size cells) 2)
      (let [qty-text (some-> (.get cells 0) .text str/trim not-empty)
            card-link (.selectFirst (.get cells 1) "a[href*=/wiki/]")
            name (some-> card-link .text str/trim not-empty)
            href (some-> card-link (.attr "href") str/trim not-empty)
            count (when-let [[_ n] (when-let [s qty-text]
                                     (re-find #"(\d+)" s))]
                    (Long/parseLong n))]
        (when-let [{:keys [expansion card-number]} (and count name href (parse-bulbapedia-card-link href))]
          {:count count
           :name name
           :expansion expansion
           :card-number card-number})))))

(defn parse-bulbapedia-deck-doc
  "Parses a Bulbapedia deck page and returns deck card entries."
  [^org.jsoup.nodes.Document doc]
  (->> (.select doc "table.roundy")
       (filter (fn [^Element table]
                 (let [header-text (some-> (.selectFirst table "tr") .text str/lower-case)]
                   (and header-text
                        (str/includes? header-text "quantity")
                        (str/includes? header-text "card")))))
       (mapcat (fn [^Element table]
                 (keep parse-bulbapedia-deck-row (.select table "tr"))))))

(defn !scrape-expansions
  "Fetches expansions from https://limitlesstcg.com/cards and returns
   a sequence of {:name :code :release-date} maps."
  []
  (let [doc (-> (Jsoup/connect cards-url)
                (.userAgent "pokedecker/0.1 (Clojure; educational scraper)")
                (.get))]
    (->> (.select doc "table.sets-table tr")
         (keep parse-expansion-row))))

(defn !scrape-expansion-cards
  "Fetches an expansion page in list view and returns
   {:number :name :type :rarity} maps."
  [expansion-code]
  (let [doc (-> (Jsoup/connect (card-list-url expansion-code))
                (.userAgent "pokedecker/0.1 (Clojure; educational scraper)")
                (.get))]
    (->> (.select doc "table.card-list tr")
         (keep parse-card-row))))

(defn !fetch-card-image-url
  "Fetches a card detail page and returns the card image URL."
  [expansion-code card-number]
  (let [doc (-> (Jsoup/connect (card-profile-url expansion-code card-number))
                (.userAgent "pokedecker/0.1 (Clojure; educational scraper)")
                (.get))]
    (parse-card-image-url doc)))

(defn !scrape-bulbapedia-deck-cards
  "Fetches a Bulbapedia deck page by deck title (e.g. \"Overgrowth\") and
   returns {:count :name :expansion :card-number} maps for cards in the deck list."
  [deck-title]
  (let [url (bulbapedia-deck-url deck-title)
        doc (-> (Jsoup/connect url)
                (.userAgent "pokedecker/0.1 (Clojure; educational scraper)")
                (.get))]
    (parse-bulbapedia-deck-doc doc)))
