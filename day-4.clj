(ns day-4 (:require hashp.core))

;byr (Birth Year) - four digits; at least 1920 and at most 2002.
;iyr (Issue Year) - four digits; at least 2010 and at most 2020.
;eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
;hgt (Height) - a number followed by either cm or in:
;If cm, the number must be at least 150 and at most 193.
;If in, the number must be at least 59 and at most 76.
;hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
;ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
;pid (Passport ID) - a nine-digit number, including leading zeroes.

(def input (->> (slurp "input4.txt")))

(defn parse-entry [[_ k w]]
  [(keyword k) w])

(defn parse [entries]
  (into {} (map parse-entry entries)))

(defn present? [passport]
  (clojure.set/subset? #{:byr :iyr :eyr :hgt :hcl :ecl :pid} (set (keys passport))))

(defn valid-hgt [hgt]
  (if-let [[_ n unit] (re-find #"(\d+)(cm|in)" hgt)]
    (case unit
      "cm" (<= 150 (Integer/parseInt n) 193)
      "in" (<= 59 (Integer/parseInt n) 76)
      false)))

(defn valid? [{:keys [byr iyr eyr hgt hcl ecl pid]}]
  (and
    (<= 1920 (Integer/parseInt byr) 2002)
    (<= 2010 (Integer/parseInt iyr) 2020)
    (<= 2020 (Integer/parseInt eyr) 2030)
    (valid-hgt hgt)
    (re-matches #"#[0-9a-f]{6}" hcl)
    (re-matches #"[0-9]{9}" pid)
    (#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} ecl)))

(defn count-matching-passports [pred in]
  (->> (clojure.string/split in #"\n\n")
       (map (partial re-seq #"(\S+):(\S+)"))
       (map parse)
       (filter pred)
       (count)))

(defn puzzle1 [in]
  (count-matching-passports present? in))

(defn puzzle2 [in]
  (count-matching-passports (every-pred present? valid?) in))

(comment (time (puzzle2 "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719
")))
(comment (time (puzzle2 input)))
(comment (time (puzzle1 "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in
")))
(comment (time (puzzle1 input)))
