;;; redux.el --- Redux implementation -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/redux.el
;; Version: 0.1.0
;; Keywords: lisp
;; Package-Requires: ((emacs "25.1"))

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

;; Redux implementation.

;;; Code:


(when (version<= "28.1" emacs-version)
  (require 'redux-shortdoc))

(defun redux-create-store (reducer &optional enhancer)
  "Create store with REDUCER and ENHANCER."
  (if enhancer
      (funcall enhancer reducer)
    (let ((listeners '())
          (state (funcall reducer nil '(:init))))
      (list
       :get-state (lambda () state)
       :dispatch (lambda (action)
                   (setq state (funcall reducer
                                        state
                                        action))
                   (mapc #'funcall listeners))
       :subscribe (lambda (new-listener)
                    (push new-listener listeners)
                    (lambda ()
                      (delete new-listener listeners)))))))

(defun redux-compose (&rest funcs)
  "Return right-to-left composition from FUNCS."
  (pcase (length funcs)
    (0 'identity)
    (1 (car funcs))
    (_
     (setq funcs (reverse funcs))
     (let ((init-fn (pop funcs)))
       (lambda (&rest args)
         (seq-reduce
          (lambda (acc fn)
            (funcall fn acc))
          funcs
          (apply init-fn args)))))))

(defun redux-combine-reducers (reducers)
  "Return a functions that invokes every reducer in REDUCERS.
REDUCERS must be plist of key and function.
Every function should accept two args - state and action and return state."
  (lambda (state action)
    (let ((changed nil)
          (next-state))
      (dotimes (idx (length reducers))
        (when (eq (logand idx 1) 0)
          (setq next-state (plist-put next-state (nth idx reducers)
                                      (funcall
                                       (plist-get
                                        reducers (nth idx reducers))
                                       (plist-get state
                                                  (nth idx reducers))
                                       action)))
          (setq changed (or changed (not (eq
                                          (plist-get state
                                                     (nth idx reducers))
                                          (plist-get next-state
                                                     (nth idx reducers))))))))
      (if changed next-state state))))

(defun redux-apply-middleware (&rest middlewares)
  "Create a store enhancer that apply MIDDLEWARES to the dispatch method."
  (lambda (reducer)
    (let ((store (funcall #'redux-create-store reducer)))
      (let* ((chain (mapcar (lambda (fn)
                              (funcall fn store))
                            middlewares))
             (dispatch (funcall (apply #'redux-compose chain)
                                (plist-get store :dispatch))))
        (setq store (plist-put store :dispatch dispatch))))))

(defmacro redux-configure (sym reducer &optional middlewares)
  "Create redux store with REDUCER and MIDDLEWARES and set it to SYM.
Also define such functions prefixed with SYM:
- dispatch
- subscribe
- get-state
- select."
  `(progn
     (defvar ,sym nil)
     (setq ,sym (redux-create-store
                 (let ((reducer ,reducer))
                   (cond ((functionp reducer)
                          reducer)
                         (t (redux-combine-reducers reducer))))
                 (when ,middlewares
                   (redux-apply-middleware
                    ,middlewares))))
     (defun ,(intern (concat (symbol-name sym) "-dispatch"))
         (&rest action)
       ,(format "Dispatch ACTION to %s." (symbol-name sym))
       (funcall (plist-get ,sym :dispatch) action))
     (defun ,(intern (concat (symbol-name sym) "-get-state"))
         (&rest action)
       ,(format "Return current state from store %s." (symbol-name sym))
       (funcall (plist-get ,sym :get-state)))
     (defun ,(intern (concat (symbol-name sym) "-subscribe"))
         (listener)
       ,(format "Subscribe LISTENER to updates in store %s." (symbol-name
                                                              sym))
       (funcall (plist-get ,sym :subscribe) listener))
     (defun ,(intern (concat (symbol-name sym) "-select"))
         (&rest path)
       ,(format "Return the value at a given PATH from store %s." (symbol-name
                                                                   sym))
       (let ((state (funcall ',(intern (concat (symbol-name sym)
                                               "-get-state"))))
             (plist)
             (value))
         (setq plist state)
         (while (and plist path (setq plist (plist-get plist (pop path))))
           (setq value plist))
         (if path
             nil
           value)))
     ,sym))

(defun redux-logger (api)
  "Simple middleware.
API should be plist created with `redux-apply-middleware'."
  (lambda (next)
    (lambda (action)
      (let ((prev-state (funcall (plist-get api :get-state))))
        (let ((value (funcall next action)))
          (print `((STATE_BEFORE  ,prev-state)
                   =>
                   (ACTION ,action)
                   =>
                   (STATE_AFTER ,(funcall (plist-get api :get-state)) )))
          value)))))

(provide 'redux)

;;; redux.el ends here