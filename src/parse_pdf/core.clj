(ns parse-pdf.core
  (:require [clojure.java.io :as io])
  (:require [parse-pdf.pdf :as pdf])
  (:require [the.parsatron :as p]))


(def output 
(let [
      f (io/file "hello.pdf")
      l (. f length)
      ]

  (with-open [in (io/input-stream f)]
    (let [buf (byte-array l)
          n (.read in buf)
          o (p/run (pdf/pdf-parser) buf)
          ]
          o ))))

