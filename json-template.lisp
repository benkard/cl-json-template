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


(defpackage #:json-template
  (:use #:common-lisp)
  (:export #:parse-template-string
           #:expand-template
           #:*template-formatters*))

(in-package #:json-template)


(defvar *template-formatters*
    `(;;("html"            . format-html)
      ;;("html-attr-value" . format-html-attr-value)
      ("raw"             . identity)))

(defun tokenize-template-string (string)
  (loop with in-command-p = nil
        with skip-newline-p = nil
        with position = 0
        for terminator = (position (if in-command-p #\} #\{) string
                                   :start position)
        collect (cons in-command-p
                      (subseq string
                              position
                              (or terminator (length string))))
        do (setq position (and terminator (1+ terminator)))
        when (and skip-newline-p
                  position
                  (< position (length string))
                  (char= (char string position) #\Newline))
          do (incf position)
        do (setq in-command-p   (not in-command-p)
                 skip-newline-p (and in-command-p
                                     position
                                     (member (char string position)
                                             (list #\. #\#))))
        until (null terminator)))

(defun parse-template-string (stream)
  (parse-raw-tokens (tokenize-template-string stream)))

(defun parse-directive (string)
  (let* ((space1 (position #\Space string))
         (space2 (and space1 (position #\Space string :start (1+ space1)))))
    (list (intern (string-upcase (subseq string 1 (or space1 (length string))))
                  '#:keyword)
          (if space1
              (subseq string (1+ space1) (or space2 (length string)))
              nil)
          (if space2
              (subseq string (1+ space2))
              nil))))

(defun parse-variable (string)
  (let ((pipe (position #\| string)))
    (if pipe
        (list (subseq string 0 pipe)
              (subseq string (1+ pipe)))
        (list string nil))))

(defun parse-token (token)
  (destructuring-bind (command-p . data)
      token
    (if command-p
        (case (char data 0)
          ((#\.)
           (list* :directive (parse-directive data)))
          ((#\#)
           (list* :comment data))
          (otherwise
           (list* :variable (parse-variable data))))
        (list :text data))))

(defun parse-raw-tokens (tokens)
  (parse-tokens (mapcar #'parse-token tokens)))

(defun parse-tokens (tokens)
  (let ((result (loop for token = (and tokens (car tokens))
                      for token-kind = (first token)
                      for token-data = (rest token)
                      until (or (null tokens)
                                (and (eq token-kind :directive)
                                     (member (second token) '(:or :end))))
                      do (setq tokens (cdr tokens))
                      if (eq token-kind :directive)
                        collect (multiple-value-bind (subresult rest-tokens)
                                    (parse-tokens tokens)
                                  (let ((sub-end-tag (first rest-tokens)))
                                    (setq tokens (rest rest-tokens))
                                    (let ((alternative
                                           (and (equal sub-end-tag '(:directive :or nil nil))
                                                (multiple-value-bind (altresult rest-tokens2)
                                                    (parse-tokens tokens)
                                                  (setq tokens (rest rest-tokens2))
                                                  altresult))))
                                      (ecase (first token-data)
                                        (:section
                                         (list :section
                                               (second token-data)
                                               subresult
                                               alternative))
                                        (:repeated
                                         (list :repeated-section
                                               (third token-data)
                                               subresult
                                               alternative))))))
                      else if (member token-kind '(:variable :text))
                        collect token
                      else if (eq token-kind :comment)
                        do (progn)
                      else do (error "Encountered invalid token: ~S" token))))
    (values result tokens)))

(defun expand-template (template context)
  (with-output-to-string (out)
    (expand-template-to-stream template (list context) out)))

(defun getcontext (context key)
  (getf context (intern (string-upcase (string key)) '#:keyword) nil))

(defun lookup-context (contexts key)
  (labels ((lookup-in-stack (context-stack)
               (if (endp context-stack)
                   nil
                   (or (getcontext (first context-stack) key)
                       (lookup-in-stack (rest context-stack))))))
    (if (string= key "@")
        (first contexts)
        (lookup-in-stack contexts))))


(defun expand-template-to-stream (template contexts stream)
  (dolist (thing template)
    (ecase (first thing)
      (:text
       (write-string (second thing) stream))
      (:variable
       (destructuring-bind (variable formatter) (cdr thing)
         (let ((value (lookup-context contexts variable)))
           (format stream "~A"
                   (if formatter
                       (funcall (cdr (assoc formatter *template-formatters*
                                            :test #'equal))
                                value)
                       value)))))
      (:section
       (destructuring-bind (section branch alternative) (cdr thing)
         (let ((value (lookup-context contexts section)))
           (expand-template-to-stream (if value branch alternative)
                                      (cons value contexts)
                                      stream))))
      (:repeated-section
       (destructuring-bind (section branch alternative) (cdr thing)
         (let ((value (lookup-context contexts section)))
           (if value
               (mapc (lambda (ctx)
                       (expand-template-to-stream branch (cons ctx contexts) stream))
                     value)
               (expand-template-to-stream alternative contexts stream))))))))
