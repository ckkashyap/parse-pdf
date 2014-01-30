(ns parse-pdf.core
  (:require [clojure.java.io :as io])
  (:require [parse-pdf.pdf :as pdf])
  (:require [the.parsatron :as p]))

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

