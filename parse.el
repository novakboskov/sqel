;;; parse.el --- parse some text to sql INSERT       -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Novak Boskov

;; Author: Novak Boskov <gnovak.boskov@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Some functions to generate ANSI SQL statements from strings.

;;; Code:
(require 'dash)
(require 's)

(defvar columns '("name" "kcal" "prot" "uh" "fat" "category"))

(defun make-sql-insert (data)
  "Make sql INSERT clause for data given in DATA list."
  (let ((data (-reduce-from (lambda (result word)
                              (concat result word)) "" data)))
    (concat "INSERT INTO foodstuffs (name, kcal, prot, uh, fat, category)\n"
            "VALUES ("
            data
            ");")))

(defun concat-with (separator &rest words)
  "Make a sentence of words dividing it by SEPARATOR.  WORDS are text to concatenate."
  (->> (-reduce-from (lambda (result word)
                       (concat result separator word)) "" words)
       (s-chop-prefix separator)))

(defun parse-nutrients (start end)
  "Parse lines for database.  START is beginning, END is end."
  (interactive "r")
  (let ((statements
         (mapcar #'make-sql-insert
                 ;; list of list consisted of data representing column
                 ;; like '('data1' 'data2' 'data3')
                 (mapcar (lambda (line)
                           (let* ((elements (split-string line " "))
                                  (len-elements (length elements))
                                  (len-columns (length columns))
                                  (words (cond ((> len-elements len-columns)
                                                ;; put together several first words
                                                (-> (-concat (list (apply #'concat-with
                                                                          (-concat '(" ")
                                                                                   (-> (- len-elements len-columns)
                                                                                       1+
                                                                                       (-split-at elements)
                                                                                       car))))
                                                             (-> (- len-elements len-columns)
                                                                 1+
                                                                 (-split-at elements)
                                                                 cdr))
                                                    -flatten))
                                               ((= (length elements) (length columns))
                                                elements))))
                             (-map-indexed (lambda (index word)
                                             (if (< index (- (length words) 1))
                                                 (concat "'" word "'" ", ")
                                               (concat "'" word "'"))) words)))
                         (split-string (buffer-substring-no-properties start end) "\n")))))
    (delete-region start end)
    (mapcar (lambda (statement)
              (insert (concat statement "\n\n"))) statements)))

(defun insert-semicolon-on-values (start end)
  "Insert semicolon after INSERT statement in region from START to END.
Inserts new line after END."
  (interactive "r")
  (let ((to-insert
         (mapcar (lambda (line)
                   (cond ((s-starts-with? "VALUES" line)
                          (if (not (s-ends-with? ";" line))
                              (concat line ";\n")
                            (concat line "\n")))
                         ((or (s-starts-with? "INSERT" line) (s-equals? line ""))
                          (concat line "\n"))
                         (t line)))
                 (split-string (buffer-substring-no-properties start end) "\n"))))
    (delete-region start end)
    (mapcar (lambda (line) (insert line)) to-insert)))

(defun shrink-this (start end)
  "Make one string of region between START and END if those strings are reparated with comma."
  (interactive "r")
  (let* ((words (split-string (buffer-substring-no-properties start end) ","))
         (region-ends-with (-last-item words)))
    (save-excursion
      (delete-region start end)
      (insert (concat "'"
                      (-reduce-from (lambda (result word)
                                      (concat result (s-replace "'" "" word)))
                                    "" words)
                      "'")))))

(provide 'parse)
;;; parse.el ends here
