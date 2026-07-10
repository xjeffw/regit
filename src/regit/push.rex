(ns regit.push
  (:require [regit.command :as regit-command :refer [regit-command regit-command-keymap]]
            [regit.util :refer [git-cmd!]]
            [rex.string :as str]
            [rex.base.theme :as theme]
            [rex.base.project :as project]
            [rex.util :as util]
            [rex.base.keys :as keys]
            [rex.base.window :as window]
            [rex.base.frame :as frame]
            [rex.base.mode :as mode]
            [rex.ui.iselect :as iselect]
            [rex.base.buffer :as buffer])
  (:use rex.core rex.builtins))

(defn- get-git-output [root & args]
  (let [result (apply git-cmd! root args)]
    (when (zero? (:code result))
      (str/trim (:out result)))))

(defn- current-branch [root]
  (try
    (git-current-branch root)
    (catch _ (get-git-output root "rev-parse" "--abbrev-ref" "HEAD"))))

(defn- get-push-remote [root branch]
  (get-git-output root "rev-parse" "--abbrev-ref" "--symbolic-full-name" (str branch "@{push}")))

(defn- get-upstream [root branch]
  (get-git-output root "rev-parse" "--abbrev-ref" "--symbolic-full-name" (str branch "@{upstream}")))

(defn- get-remote-branches [root]
  (try
    (let [branches (git-remote-branches root)]
      (vec (remove str/blank? branches)))
    (catch _ [])))

(defn- run-git-push [root args-values remote-target branch]
  (let [force? (:force-with-lease args-values)
        branch (or branch (current-branch root))
        cmd-args (cond-> ["push"]
                   force? (conj "--force-with-lease"))
        cmd-args (if remote-target
                   (let [parts (str/split remote-target #"/")
                         remote (first parts)
                         ref (str/join "/" (rest parts))
                         refspec (if (str/blank? ref)
                                   branch
                                   (str branch ":" ref))]
                     (into cmd-args [remote refspec]))
                   cmd-args)
        start-window (focused-window)]
    (message (str "Running: git " (str/join " " cmd-args)))
    (future
      (with-buffer-pending (call-var regit.status/find-status-buffer root)
        (let [{:keys [code out err]} (apply git-cmd! root cmd-args)]
          (let [details (->> [err out]
                          (map str/trim)
                          (remove str/blank?)
                          (str/join "\n"))]
            (if (zero? code)
              (do
                (if (str/index-of details "Everything up-to-date")
                  (message "Everything up-to-date")
                  (let [branch (or branch (current-branch root))
                        [remote ref] (when remote-target
                                       (let [parts (str/split remote-target #"/")]
                                         [(first parts) (str/join "/" (rest parts))]))
                        target (if remote
                                 (str remote "/" (if (str/blank? ref) branch ref))
                                 "default remote")]
                    (message (str "Pushed branch " branch " to " target))))
                (let [should-focus? (= (focused-window) start-window)]
                  (or (call-var regit.status/refresh-status! root should-focus?)
                    (call-var regit.status/regit-status root should-focus?)))
                nil)
              (let [msg (if (str/blank? details)
                          (str "Push failed (exit " code ")")
                          (str "Push failed: " details))]
                (message msg)
                msg))))))))

(defn ^:interactive regit-push []
  (if-let [root (project/current-project-root)]
    (let [branch (current-branch root)
          push-remote (get-push-remote root branch)
          upstream (get-upstream root branch)]
      (regit-command
        {:args {:force-with-lease {:label "-f Force with lease (--force-with-lease)"
                                   :key "- f"
                                   :value false}}
         :return-window (focused-window)
         :actions {"p" {:label (or push-remote "pushRemote")
                        :fn (fn [args] (run-git-push root args push-remote branch))}
                   "u" {:label (or upstream "upstream")
                        :fn (fn [args] (run-git-push root args upstream branch))}
                   "e" {:label "elsewhere"
                        :fn (fn [args]
                              (let [entries (get-remote-branches root)
                                    entries-fn (fn [_input] entries)
                                    submit-fn (fn [input selected-entry]
                                                (let [input (str/trim (str input))
                                                      selected (when selected-entry (str selected-entry))
                                                      target (if selected selected input)]
                                                  (if (str/blank? (or target ""))
                                                    "No target selected"
                                                    (run-git-push root args target branch))))]
                                (iselect/iselect
                                  (str "Push " branch " to elsewhere:")
                                  entries-fn
                                  (fn [_] nil)
                                  {:complete-fn #'iselect/complete-to-first-and-select
                                   :submit-fn submit-fn
                                   :sync-input-on-move? false
                                   :preserve-selection? true
                                   :clear-selection-on-input? true})))}}
         :layout ["Arguments"
                  {:arg :force-with-lease}
                  ""
                  (str "Push " branch " to")
                  {:action "p"}
                  {:action "u"}
                  {:action "e"}]}))
    (message "Not in a git repository")))
