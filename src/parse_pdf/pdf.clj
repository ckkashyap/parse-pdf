(ns parse-pdf.pdf
  (:require [the.parsatron :as p]))

(p/defparser optional [p default-value] 
  (p/either (p/attempt p) (p/always default-value))) 

; delimiters ( ) < > [ ] { } / %

(def whitespace-set #{0 9 10 12 13 32})

(def delimiter-set #{37 40 41 47 60 62 91 93 123 125})

(defn non-newline
  []
  (p/token #(and (not= % 10) (not= % 13 ))))

(defn whitespace-parser
  []
  (p/token #(whitespace-set %)))


(p/defparser delimiter-parser []
  (p/token #(delimiter-set %)))

(p/defparser neither-delimiter-nor-whitespace []
  (p/token #(not (or (whitespace-set %) (delimiter-set %)))))
             
(defn digit
  "Consume a digit [0-9] character"
  []
  (p/token #(and (>= % 48) (<= % 57 ))))


(defn number-parser []
  (p/let->> [ num_vec (p/many1 (digit)) ]
            (let [ num_str (apply str (map char num_vec)) ]
              (p/always (Integer/parseInt num_str)))))

(defn string-to-byte-vector [str]
  (into [] (map int str)))

(p/defparser pdf-newline-parser []
  (p/choice
   (p/string (string-to-byte-vector "\r\n"))
   (p/char 10)))

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
             _n1 (optional (number-parser) nil)
             _n2 (optional (-float-parser) nil)
             ] (if _n2 (p/always (Float/parseFloat (str _sign _n1 "." _n2))) (if _n1 (p/always (Integer/parseInt (str _sign _n1))) (p/never)))))
            
(p/defparser -char-parser []
  (p/let->> [
             _e (optional (p/char 92) nil)
             _c (if _e (p/token #(#{41 92} %)) (p/token #(not= % 41)))
             ] (p/always _c)))
             
(p/defparser pdf-string-parser []
  (p/let->> [
             _ (p/many (whitespace-parser))
             _ (p/char 40)
             _str (p/many (-char-parser))
             _ (p/char 41)
             ] (p/always {:type :string :data (apply str (map char _str))})))

(p/defparser pdf-name-parser []
  (p/let->> [
             _ (p/many (whitespace-parser))
             _ (p/char 47)
             _str (p/many (neither-delimiter-nor-whitespace))
             ] (p/always {:type :name :data (apply str (map char _str))})))

  

(p/defparser pdf-indirect-reference []
  (p/let->> [
             _ (p/many (whitespace-parser))
             object-number (number-parser)
             _ (p/many (whitespace-parser))
             object-generation (number-parser)
             _ (p/many (whitespace-parser))
            _ (p/char (int \R))
             _ (p/many (whitespace-parser))
             ] (p/always {:type :reference :ref-object-number object-number :ref-object-generation object-generation})))
  


(p/defparser pdf-dictionary-key-value []
  (p/let->> [
             _key (pdf-name-parser)
             _ (p/many (whitespace-parser))
             _value (pdf-object)
             _ (p/many (whitespace-parser))
             ] (p/always {(:data _key) _value})))
  
  

(p/defparser pdf-dictionary []
  (p/let->> [
             _ (p/string (string-to-byte-vector "<<"))
             _key_values (p/many1 (pdf-dictionary-key-value))
             _ (p/string (string-to-byte-vector ">>"))
             ] (p/always {:type :dictionary  :data  (into {} _key_values)})))
                     


(p/defparser pdf-array []
  (p/let->> [
             _ (p/char (int \[))
             _values (p/many1 (pdf-object))
             _ (p/many (whitespace-parser))
             _ (p/char (int \]))
             ] (p/always (into [] _values))))
                     



(defn pdf-stream-length [body]
  (if (= :dictionary (:type body))
    ((:data body) "Length")
    false))


(p/defparser pdf-stream-parser
  [body]
  (p/let->> [
             _ (p/string (string-to-byte-vector "stream"))
             _ (pdf-newline-parser)
             _stream (p/times (pdf-stream-length body) (p/token #(not= nil %)))
             b  (p/string (string-to-byte-vector "endstream"))        
        ]
  (if true (p/always [123 _stream])  (p/always false))))


(p/defparser pdf-indirect-object []
  (p/let->> [
             _ (p/many (whitespace-parser))
             object-number (number-parser)
             _ (p/many (whitespace-parser))
             object-generation (number-parser)
             _ (p/many (whitespace-parser))
             _ (p/string (string-to-byte-vector "obj"))
             _ (p/many (whitespace-parser))
             object-body (pdf-object)
             _ (p/many (whitespace-parser))
             object-stream (optional (pdf-stream-parser object-body) nil)
             _ (p/many (whitespace-parser))
             _ (p/string (string-to-byte-vector "endobj"))
             ]
            (p/always {:type :indirect-object :object-number object-number :object-genertion object-generation :object-body object-body :object-stream object-stream})))


(p/defparser pdf-object []
  (p/choice
             (p/attempt (pdf-comment))
             (p/attempt (pdf-indirect-object))
             (p/attempt (pdf-indirect-reference))
             (p/attempt (pdf-name-parser))
             (p/attempt (pdf-dictionary))
             (p/attempt (pdf-array))
             (p/attempt (pdf-numeric-parser))
             ))
 

(p/defparser pdf-body-parser [header]
 (p/let->> [
             _ (p/many (whitespace-parser))
            o1 (pdf-object)
             _ (p/many (whitespace-parser))
             o2 (pdf-object)
             _ (p/many (whitespace-parser))
            o3 (pdf-object)
 
            ] (p/always [o1 o2 o3])))


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
