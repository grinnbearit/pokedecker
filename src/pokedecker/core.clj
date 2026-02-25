(ns pokedecker.core
  (:require [clojure.string :as str])
  (:import (java.time LocalDate)
           (java.time.format TextStyle)
           (java.util Locale)
           (org.jsoup Jsoup)
           (org.jsoup.nodes Element)))

(def ^:private cards-url "https://limitlesstcg.com/cards")

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

(defn- cleaned-text
  "Returns text content after removing decorative nodes matched by selector."
  [^Element el selector]
  (when el
    (let [copy (.clone el)]
      (.remove (.select copy selector))
      (some-> (.text copy) str/trim not-empty))))

(defn parse-card-row
  "Parses a single card list <tr> into {:number :name :type}.
   Returns nil for header/malformed rows."
  [^Element tr]
  (let [cells (.select tr "td")]
    (when (>= (.size cells) 4)
      (let [number (some-> (.get cells 1) .text str/trim not-empty)
            name (some-> (.get cells 2) .text str/trim not-empty)
            type (cleaned-text (.get cells 3) ".ptcg-symbol")]
        (when (and number name)
          {:number number
           :name name
           :type type})))))

(defn parse-card-image-url
  "Parses a card detail page and returns the card image URL.
   Prefers the high-res `data-src` attribute when present."
  [^org.jsoup.nodes.Document doc]
  (when-let [img (.selectFirst doc "div.card-image img")]
    (or (some-> (.attr img "data-src") str/trim not-empty)
        (some-> (.attr img "src") str/trim not-empty))))

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
   {:number :name :type} maps."
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
