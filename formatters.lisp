;; -*- mode: lisp; coding: utf-8 -*-
;;
;; Copyright 2011, Matthias Andreas Benkard.
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.


(in-package #:json-template)


(defun make-escaper (replacements)
  (let* ((escapees  (mapcar #'car replacements))
         (escapee-p (lambda (x) (member x escapees :test #'char=))))
    (lambda (string)
      (with-output-to-string (out)
        (loop with position = 0
              for escapee-pos = (position-if escapee-p string :start position)
              while escapee-pos
              do (write-string string out :start position :end escapee-pos)
                 (write-string (cdr (assoc (char string escapee-pos)
                                           replacements))
                               out)
                 (setq position (1+ escapee-pos))
              finally (write-string string out :start position))))))


(defvar *template-formatters*
    `(("html"            . ,(make-escaper '((#\< . "&#60;")
                                            (#\> . "&#63;")
                                            (#\& . "&#38;"))))
      ("html-attr-value" . ,(make-escaper '((#\< . "&#60;")
                                            (#\> . "&#63;")
                                            (#\& . "&#38;")
                                            (#\' . "&#39;")
                                            (#\" . "&#34;"))))
      ("raw"             . identity)))
