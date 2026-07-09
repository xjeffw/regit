(ns regit.synthetic
  (:require [rex.base.mode :as mode]
            [rex.base.keys :refer [make-keymap map!]])
  (:use rex.core rex.builtins))

(def regit-synthetic-keymap (make-keymap))

(map! :map regit-synthetic-keymap
  ("q" #'close-buffer))

(def regit-synthetic-keymaps
  [regit-synthetic-keymap])

(mode/register-submode :regit-synthetic
  {:name :regit-synthetic
   :keymaps regit-synthetic-keymaps})
