;; -*- no-byte-compile: t; -*-
;;; redux-shortdoc.el --- Examples of usage redux.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/redux.el
;; Version: 0.1.0
;; Keywords: lisp
;; Package-Requires: ((emacs "28.1"))

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

;; Examples of usage redux.el

;;; Code:

(when (version<= "28.1" emacs-version)
  (require 'shortdoc)
  (define-short-documentation-group redux
    "Redux usage."
    (redux-combine-reducers
     :eval
     (progn
       (require 'redux)
       (defun redux-shortdoc-example-counter-reducer (state action)
         "Increment, decrement or set STATE depending of ACTION."
         (pcase (car action)
           (:inc (1+ (or state 0)))
           (:dec (1- (or state 0)))
           (:set (plist-get action :set))
           (_ (or state 0))))
       (defvar redux-shortdoc-root-reducer
         (redux-combine-reducers
          (list
           :counter
           'redux-shortdoc-example-counter-reducer)))))
    (redux-apply-middleware
     :eval
     (defvar redux-shortdoc-enhancer (redux-apply-middleware 'redux-logger)))
    (redux-create-store
     :eval
     (defvar redux-shortdoc-example-store (redux-create-store
                                           redux-shortdoc-root-reducer
                                           redux-shortdoc-enhancer))
     (defun redux-shortdoc-dispatch (&rest action)
       "Dispatch ACTION."
       (funcall (plist-get redux-shortdoc-example-store :dispatch) action))
     (redux-shortdoc-dispatch :set 5)
     (redux-shortdoc-dispatch :inc)
     (redux-shortdoc-dispatch :inc)
     (funcall (plist-get redux-shortdoc-example-store :get-state)))))

(eval-when-compile
  (when (version< emacs-version "28.1")
    (warn "Emacs should not be compiling this file")))

(provide 'redux-shortdoc)
;;; redux-shortdoc.el ends here