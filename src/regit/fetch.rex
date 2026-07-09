(ns regit.fetch
  (:require [regit.command :as regit-command :refer [regit-command]]
            [rex.string :as str]
            [rex.base.project :as project]
            [rex.ui.iselect :as iselect]
            [rex.base.buffer :as buffer])
  (:use rex.core rex.builtins))

(defn- git-cmd [root & args]
  (run-shell* "git" (into ["-C" (str root)] args)))

(defn- get-git-output [root & args]
  (let [result (apply git-cmd root args)]
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

(defn- extract-remote [target]
  (when target
    (first (str/split target #"/"))))

(defn- get-remotes [root]
  (try
    (let [out (get-git-output root "remote")]
      (if (str/blank? out) [] (str/split-lines out)))
    (catch _ [])))

(defn- run-git-fetch [root target]
  (let [cmd-args (cond-> ["fetch"]
                   (= target :all) (conj "--all")
                   (and target (not= target :all)) (conj (first (str/split target #"/"))))
        start-window (focused-window)]
    (message (str "Running: git " (str/join " " cmd-args)))
    (future
      (with-buffer-pending (call-var regit.status/find-status-buffer root)
        (let [{:keys [code out err]} (apply git-cmd root cmd-args)]
          (let [details (->> [err out]
                          (map str/trim)
                          (remove str/blank?)
                          (str/join "\n"))]
            (if (zero? code)
              (do
                (message (str "Fetched " (if (= target :all) "all remotes" (or (extract-remote target) "default remote"))))
                (let [should-focus? (= (focused-window) start-window)]
                  (or (call-var regit.status/refresh-status! root should-focus?)
                    (call-var regit.status/regit-status root should-focus?)))
                nil)
              (let [msg (if (str/blank? details)
                          (str "Fetch failed (exit " code ")")
                          (str "Fetch failed: " details))]
                (message msg)
                msg))))))))

(defn ^:interactive regit-fetch []
  (if-let [root (project/current-project-root)]
    (let [branch (current-branch root)
          push-remote (get-push-remote root branch)
          upstream (get-upstream root branch)]
      (regit-command
        {:args {}
         :return-window (focused-window)
         :actions {"p" {:label (or (extract-remote push-remote) "pushRemote")
                        :fn (fn [_args] (run-git-fetch root push-remote))}
                   "u" {:label (or (extract-remote upstream) "upstream")
                        :fn (fn [_args] (run-git-fetch root upstream))}
                   "e" {:label "elsewhere"
                        :fn (fn [_args]
                              (let [entries (get-remotes root)
                                    entries-fn (fn [_input] entries)
                                    submit-fn (fn [input selected-entry]
                                                (let [target (or selected-entry input)]
                                                  (if (str/blank? (or target ""))
                                                    "No remote selected"
                                                    (run-git-fetch root target))))]
                                (iselect/iselect
                                  "Fetch from remote:"
                                  entries-fn
                                  (fn [_] nil)
                                  {:complete-fn #'iselect/complete-to-first-and-select
                                   :submit-fn submit-fn})))}
                   "a" {:label "all remotes"
                        :fn (fn [_args] (run-git-fetch root :all))}}
         :layout ["Fetch from"
                  {:action "p"}
                  {:action "u"}
                  {:action "e"}
                  {:action "a"}]}))
    (message "Not in a git repository")))
