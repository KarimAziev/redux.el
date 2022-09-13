;;; example.el --- Example of usage redux.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/redux.el
;; Version: 0.1.0
;; Keywords: lisp
;; Package-Requires: ((emacs "24.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Example of usage redux.el

;;; Code:

(require 'redux)

(defun example-counter-reducer (state action)
  "Increment, decrement or set STATE depending of ACTION."
  (pcase (car action)
    (:inc (1+ (or state 0)))
    (:dec (1- (or state 0)))
    (:set (plist-get action :set))
    (_ (or state 0))))

(defvar example-store (redux-create-store
                       (redux-combine-reducers
                        (list
                         :counter 'example-counter-reducer))
                       (redux-apply-middleware
                        'redux-logger)))

(defun example-dispatch (&rest action)
  "Dispatch ACTION."
  (funcall (plist-get example-store :dispatch) action))

(defun example-get-state ()
  "Return current state."
  (let ((state (funcall (plist-get example-store :get-state))))
    state))

(defun example-subscribe (listener)
  "Subribe LISTENER to `example-store'."
  (funcall (plist-get example-store :subscribe) listener))

(defun example-subscriber ()
  "Subribe to redux update."
  (message "state changed %s"
           (funcall (plist-get example-store :get-state))))

(example-dispatch :inc)
(example-dispatch :inc)
(example-dispatch :sff)
(print (funcall (plist-get example-store :get-state)))
(provide 'example)
;;; example.el ends here