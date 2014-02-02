(ns parse-pdf.pdf
  (:require [the.parsatron :as p]))

(p/defparser optional [p default-value] 
  (p/either (p/attempt p) (p/always default-value))) 

; delimiters ( ) < > [ ] { } / %

(defn non-newline
  []
  (p/token #(and (not= % 10) (not= % 13 ))))

(defn whitespace-parser
  []
  (p/token #(#{0 9 10 12 13 32} %)))


(defn digit
  "Consume a digit [0-9] character"
  []
  (p/token #(and (>= % 48) (<= % 57 ))))


(defn number-parser []
  (p/let->> [ num_vec (p/many1 (digit)) ]
            (let [ num_str (apply str (map char num_vec)) ]
              (p/always num_str))))

(defn string-to-byte-vector [str]
  (into [] (map int str)))

(p/defparser pdf-header-parser []
  (p/let->> [
             _ (p/char (int \%))
             signature (p/string (string-to-byte-vector "PDF"))
             _ (p/char (int \-))
             major-version (number-parser)
             _ (p/char (int \.))
             minor-version (number-parser)
             ]
            (p/always {:major-version major-version, :minor-version minor-version})))

(p/defparser pdf-comment []
  (p/let->> [
             _ (p/many (whitespace-parser))
             _ (p/char (int \%))
             pdf-comment-text (p/many (non-newline))
             ]
            (p/always {:pdf-comment pdf-comment-text})))


(def pdf-object)

(p/defparser pdf-boolean-parser []
  (p/let->> [
             _ (p/many (whitespace-parser))
             _t (optional (p/string (string-to-byte-vector "true")) nil)
             _f (optional (p/string (string-to-byte-vector "false")) nil)
             ] (if _t (p/always true) (if _f (p/always false) nil))))
           
(p/defparser -sign-parser []
  (p/let->> [
             _ (p/many (whitespace-parser))             
            _minus (optional (p/char 45) nil)
            _plus (optional (p/char 43) nil)
             ]
            (if _minus (p/always "-") (p/always "+"))))

(p/defparser -float-parser []
  (p/let->> [
             _ (p/char 46)
             _n (number-parser)
             ] (p/always _n)))

(p/defparser pdf-numeric-parser []
  (p/let->> [
             _sign (-sign-parser)
             _n1 (optional (number-parser) 0)
             _n2 (optional (-float-parser) nil)
             ] (if _n2 (p/always (str _sign _n1 "." _n2)) (p/always (str _sign _n1)))))
            
             
  

;(p/defparser pdf-name-parser []
  

;(p/defparser pdf-dictionary []
;  (p/let->> [
;             _ (p/string (string-to-byte-vector "<<"))
             

(p/defparser pdf-indirect-object []
  (p/let->> [
             _ (p/many (whitespace-parser))
             object-number (number-parser)
             _ (p/many (whitespace-parser))
             object-generation (number-parser)
             _ (p/many (whitespace-parser))
             _ (p/string (string-to-byte-vector "obj"))
             object-body (pdf-object)
             _ (p/many (whitespace-parser))
             _ (p/string (string-to-byte-vector "endobj"))
             ]
            (p/always {:object-number object-number :object-generation object-generation :object-body object-body})))



(p/defparser pdf-object []
  (p/let->> [
             c (optional (pdf-comment) nil)
             object (optional (pdf-indirect-object) nil)
             ]
            (p/always {:object object :comment c})))


(p/defparser pdf-body-parser [header]
 (p/let->> [
            o1 (pdf-object)
            ;o2 (pdf-object)
            ] (p/always [o1])))


(p/defparser pdf-xref-table-parser [header body]
   (p/always true))
(p/defparser pdf-trailer-parser [header body xref-table]
   (p/always true))


(p/defparser pdf-parser []
  (p/let->> [
             pdf-header     (pdf-header-parser)
             pdf-body       (pdf-body-parser pdf-header)
             pdf-xref-table (pdf-xref-table-parser pdf-header pdf-body)
             pdf-trailer    (pdf-trailer-parser pdf-header pdf-body pdf-xref-table)
             ]
            (p/always [pdf-header pdf-body pdf-xref-table pdf-trailer])
))
