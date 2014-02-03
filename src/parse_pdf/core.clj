(ns parse-pdf.core
  (:require [clojure.java.io :as io])
  (:require [parse-pdf.pdf :as pdf])
  (:require [the.parsatron :as p]))




(println (p/run (pdf/pdf-boolean-parser) (pdf/string-to-byte-vector " false")))
(println (p/run (pdf/pdf-numeric-parser) (pdf/string-to-byte-vector " 1")))
(println (p/run (pdf/pdf-numeric-parser) (pdf/string-to-byte-vector " +1")))
(println (p/run (pdf/pdf-numeric-parser) (pdf/string-to-byte-vector " -0.0")))
(println (p/run (pdf/pdf-numeric-parser) (pdf/string-to-byte-vector " -.1")))
(println (p/run (pdf/pdf-numeric-parser) (pdf/string-to-byte-vector " -10")))

(println (p/run (pdf/pdf-string-parser) (pdf/string-to-byte-vector "(slkfdlkj\\)  \\\\ d)")))

(println (p/run (pdf/pdf-name-parser) (pdf/string-to-byte-vector "/Hel[lo![")))



(let [
      f (io/file "hello.pdf") ;  the file contains "abcdefg"
      l (. f length)
      ]

  (with-open [in (io/input-stream f)]
    (let [buf (byte-array l)
          n (.read in buf)
          output (p/run (pdf/pdf-parser) buf)
          ]
      (println "output = " (:errmsg output) output)
      (println n)))) ; (99 100 101 102 103 13 10)

