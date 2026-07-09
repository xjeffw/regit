(ns regit.merge
  (:require [regit.command :as regit-command :refer [regit-command]]
            [regit.util :as regit-util :refer [with-status-buffer-pending]]
            [rex.ui.iselect :as iselect]
            [rex.base.hook :refer [run-hooks]]
            [rex.base.keys :refer [make-keymap map!]]
            [rex.base.mode :refer [register-mode activate-mode]]
            [rex.base.project :as project]
            [rex.base.buffer :as buffer]
            [rex.base.theme :as theme]
            [rex.string :as str])
  (:use rex.core rex.builtins))

(def merge-strategies ["resolve" "recursive" "octopus" "ours" "subtree"])

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

(defn- branch-exists? [root branch]
  (zero? (:code (git-cmd root "show-ref" "--verify" "--quiet" (str "refs/heads/" branch)))))

(defn- git-dir [root]
  (let [dir (get-git-output root "rev-parse" "--git-dir")]
    (when-not (str/blank? (or dir ""))
      (if (str/starts-with? dir "/")
        dir
        (path-join root dir)))))

(defn- git-file [root file]
  (when-let [dir (git-dir root)]
    (path-join dir file)))

(defn- merge-in-progress? [root]
  (let [path (git-file root "MERGE_HEAD")]
    (and path (path-exists? path))))

(defn- result-details [result]
  (->> [(:err result) (:out result)]
    (map str/trim)
    (remove str/blank?)
    (str/join "\n")))

(defn- git-error-message [operation result]
  (let [details (result-details result)]
    (if (str/blank? details)
      (str operation " failed (exit " (:code result) ")")
      (str operation " failed: " details))))

(defn- notify-project-file-git-change! [root]
  (doseq [buf (project/project-file-buffers root)]
    (when-let [file (:file buf)]
      (binding [*buffer* buf]
        (run-hooks buffer/on-buffer-file-git-change file)))))

(defn- refresh-status! [root start-window]
  (when (call-var regit.status/find-status-buffer root)
    (let [should-focus? (if start-window (= (focused-window) start-window) nil)]
      (or (call-var regit.status/refresh-status! root should-focus?)
        (call-var regit.status/regit-status root should-focus?)))))

(defn- after-git-change! [root start-window]
  (git-refresh-index root)
  (git-clear-repo-cache)
  (notify-project-file-git-change! root)
  (refresh-status! root start-window))

(defn- run-git-result! [root operation result opts]
  (let [start-window (:start-window opts)
        success-message (:success-message opts)
        refresh? (not= false (:refresh? opts))
        success? (zero? (:code result))]
    (when refresh?
      (after-git-change! root start-window))
    (if success?
      (do
        (when-not (str/blank? (or success-message ""))
          (message success-message))
        nil)
      (let [msg (git-error-message operation result)]
        (message msg)
        msg))))

(defn- run-git! [root operation args & [opts]]
  (let [opts (or opts {})]
    (with-status-buffer-pending root
      (run-git-result! root operation (apply git-cmd root args) opts))))

(defn- run-git-with-commit-editor! [root operation env args & [opts]]
  (let [opts (or opts {})]
    (regit-util/run-git-with-commit-editor! root operation env args
      (assoc opts
        :on-result (fn [result]
                     (run-git-result! root operation result opts))
        :on-abort (fn []
                    (after-git-change! root (:start-window opts)))))))

(defn- ref-lines [root & args]
  (let [out (apply get-git-output root args)]
    (vec (->> (str/split (or out "") #"\r?\n")
           (map str/trim)
           (remove str/blank?)))))

(defn- local-branches [root]
  (ref-lines root "for-each-ref" "--format=%(refname:short)" "refs/heads"))

(defn- remote-branches [root]
  (vec (->> (ref-lines root "for-each-ref" "--format=%(refname:short)" "refs/remotes")
         (remove #(str/ends-with? % "/HEAD")))))

(defn- other-local-branches [root]
  (let [current (current-branch root)]
    (vec (remove #(= % current) (local-branches root)))))

(defn- merge-targets [root]
  (let [current (current-branch root)]
    (vec (->> (concat (local-branches root) (remote-branches root))
           (remove #(= % current))
           distinct))))

(defn- selected-target [input selected-entry]
  (let [input (str/trim (str input))
        selected (when selected-entry (str selected-entry))]
    (if (str/blank? (or selected ""))
      input
      selected)))

(defn- read-git-file-lines [root file]
  (let [path (git-file root file)]
    (if (and path (path-exists? path))
      (vec (->> (str/split (try (read-file path) (catch _ "")) #"\r?\n")
             (map str/trim)
             (remove str/blank?)))
      [])))

(defn- short-ref-name [root rev]
  (or (when-not (str/blank? (or rev ""))
        (get-git-output root "name-rev" "--name-only" "--no-undefined" rev))
    rev))

(defn- parse-log-commits [rendered]
  (if (str/blank? (or rendered ""))
    []
    (->> (str/split rendered #"\u001e")
      (map str/trim)
      (remove str/blank?)
      (mapv (fn [record]
              (let [parts (str/split record #"\u001f")
                    refs (nth parts 1 "")]
                {:id (nth parts 0 "")
                 :refs (if (str/blank? refs)
                         []
                         (mapv str/trim (str/split refs #",")))
                 :summary (nth parts 2 "")}))))))

(defn- merge-log-commits [root range]
  (if (str/blank? (or range ""))
    []
    (parse-log-commits
      (get-git-output root
        "log"
        range
        "--decorate=short"
        "--format=%h%x1f%D%x1f%s%x1e"))))

(defn- ref-text [text]
  (theme/with-face text :regit-ref))

(defn- branch-text [text]
  (theme/with-face text :regit-branch))

(defn- hash-text [text]
  (theme/with-face text :regit-hash))

(defn- symbolic-head? [ref]
  (or (= ref "HEAD")
    (str/ends-with? ref "/HEAD")))

(defn- render-ref [ref]
  (cond
    (str/includes? ref " -> ")
    (let [[source target] (str/split ref #" -> " 2)]
      (str
        (if (symbolic-head? source)
          (ref-text source)
          (branch-text source))
        (ref-text " -> ")
        (branch-text target)))

    (symbolic-head? ref) (ref-text ref)
    :else (branch-text ref)))

(defn- render-refs [refs]
  (when (seq refs)
    (str
      (ref-text "[")
      (str/join (ref-text ", ") (mapv render-ref refs))
      (ref-text "] "))))

(defn- merge-commit-line [commit]
  (str (hash-text (:id commit)) " " (or (render-refs (:refs commit)) "") (:summary commit)))

(defn merge-status [root]
  (when (merge-in-progress? root)
    (let [heads (read-git-file-lines root "MERGE_HEAD")
          labels (mapv #(short-ref-name root %) heads)
          first-head (first heads)
          base (when first-head
                 (get-git-output root "merge-base" "--octopus" "HEAD" first-head))
          range (when (and first-head (not (str/blank? (or base ""))))
                  (str base ".." first-head))
          commits (merge-log-commits root range)]
      {:heading (str "Merging " (if (seq labels) (str/join ", " labels) "HEAD") ":")
       :commits (mapv (fn [commit]
                        {:id (:id commit)
                         :text (merge-commit-line commit)})
                  commits)})))

(defn- select-target [prompt entries on-select]
  (let [entries (vec entries)
        submit-fn (fn [input selected-entry]
                    (let [target (selected-target input selected-entry)]
                      (if (str/blank? (or target ""))
                        (do
                          (message "No revision selected")
                          :keep-open)
                        (fn []
                          (let [result (on-select target)]
                            (when (string? result)
                              (message result))
                            result)))))]
    (iselect/iselect
      prompt
      (fn [_input] entries)
      (fn [_entry] nil)
      {:complete-fn #'iselect/complete-to-first-and-select
       :submit-fn submit-fn
       :sync-input-on-move? false
       :preserve-selection? true
       :clear-selection-on-input? true})))

(defn- merge-args [args-values]
  (cond-> []
    (:ff-only args-values) (conj "--ff-only")
    (:no-ff args-values) (conj "--no-ff")
    (not (str/blank? (or (:strategy args-values) "")))
    (conj (str "--strategy=" (:strategy args-values)))))

(defn- force-no-ff-args [args-values]
  (assoc args-values :ff-only false :no-ff true))

(defn- merge-command-args [args-values prefix suffix]
  (vec (concat ["merge"] prefix (merge-args args-values) suffix)))

(defn run-merge-plain! [root args-values rev]
  (let [args (merge-command-args args-values ["--no-edit"] [rev])]
    (run-git! root
      "Merge"
      args
      {:success-message (str "Merged " rev)
       :start-window (focused-window)})))

(defn run-merge-editmsg! [root args-values rev return-window]
  (let [args (merge-command-args (force-no-ff-args args-values) ["--edit"] [rev])]
    (run-git-with-commit-editor! root
      "Merge"
      {}
      args
      {:success-message "Merged"
       :start-window return-window
       :abort-message "Aborted merge commit message edit"})))

(defn run-merge-nocommit! [root args-values rev]
  (let [args (merge-command-args (force-no-ff-args args-values) ["--no-commit"] [rev])]
    (run-git! root
      "Merge"
      args
      {:success-message (str "Merged " rev " without committing")
       :start-window (focused-window)})))

(defn run-merge-squash! [root _args-values rev]
  (run-git! root
    "Squash merge"
    ["merge" "--squash" rev]
    {:success-message (str "Squash merged " rev)
     :start-window (focused-window)}))

(defn run-merge-absorb! [root args-values branch]
  (cond
    (= branch (current-branch root))
    "Cannot absorb the current branch"

    (not (branch-exists? root branch))
    (str "No local branch named " branch)

    :else
    (let [args (merge-command-args args-values ["--no-edit"] [branch])
          merge-err (run-git! root
                      "Absorb"
                      args
                      {:success-message (str "Merged " branch)
                       :refresh? false})]
      (if merge-err
        merge-err
        (run-git! root
          "Delete branch"
          ["branch" "-D" branch]
          {:success-message (str "Absorbed " branch)
           :start-window (focused-window)})))))

(defn run-merge-dissolve! [root args-values branch]
  (let [current (current-branch root)
        head (get-git-output root "rev-parse" "HEAD")]
    (cond
      (= branch current)
      "Cannot dissolve into the current branch"

      (not (branch-exists? root branch))
      (str "No local branch named " branch)

      :else
      (let [checkout-err (run-git! root
                           "Checkout"
                           ["checkout" branch]
                           {:success-message (str "Checked out " branch)
                            :refresh? false})]
        (if checkout-err
          checkout-err
          (if (and current (not= current "HEAD"))
            (run-merge-absorb! root args-values current)
            (run-git! root
              "Dissolve"
              (merge-command-args args-values ["--no-edit"] [head])
              {:success-message "Dissolved detached HEAD"
               :start-window (focused-window)})))))))

(defn- merge-tree-output [root rev]
  (let [base (get-git-output root "merge-base" "HEAD" rev)
        args (if (str/blank? (or base ""))
               ["merge-tree" "HEAD" rev]
               ["merge-tree" base "HEAD" rev])]
    (apply git-cmd root args)))

(defn- merge-preview-buffer-name [root rev]
  (str "*regit-merge-preview: " (or (path-filename root) root) " " rev "*"))

(defn ^:interactive regit-merge-preview-quit []
  (let [return-window (:return-window @(buffer-state *buffer*))]
    (close-buffer)
    (when return-window
      (set-focused-window return-window))))

(def merge-preview-keymap (make-keymap))

(map! :map merge-preview-keymap
  ("q" #'regit-merge-preview-quit))

(register-mode :regit-merge-preview
  {:name :regit-merge-preview
   :icon "󰊢 "
   :keymaps [#'merge-preview-keymap]
   :submodes [:vim]})

(defn run-merge-preview! [root _args-values rev]
  (let [return-window (focused-window)
        result (merge-tree-output root rev)]
    (if (zero? (:code result))
      (let [buffer (create-buffer true)
            window (or return-window (focused-window))
            content (if (str/blank? (:out result))
                      "(merge preview produced no output)"
                      (:out result))]
        (set-window-buffer buffer window)
        (set-focused-window window)
        (binding [*buffer* buffer
                  *window* window]
          (set-buffer-name (merge-preview-buffer-name root rev) buffer)
          (activate-mode :regit-merge-preview)
          (swap! (buffer-state buffer) assoc
            :project-root root
            :regit-root root
            :merge-rev rev
            :return-window return-window)
          (set-string content buffer)
          (set-buffer-read-only true buffer)
          (move-cursor 0))
        nil)
      (let [msg (git-error-message "Preview merge" result)]
        (message msg)
        msg))))

(defn run-merge-abort! [root]
  (if (merge-in-progress? root)
    (run-git! root
      "Abort merge"
      ["merge" "--abort"]
      {:success-message "Aborted merge"
       :start-window (focused-window)})
    "No merge in progress"))

(defn run-merge-commit! [root return-window]
  (if (merge-in-progress? root)
    (run-git-with-commit-editor! root
      "Merge commit"
      {}
      ["commit"]
      {:success-message "Merged"
       :start-window return-window
       :abort-message "Aborted merge commit message edit"})
    "No merge in progress"))

(defn- command-args []
  {:ff-only {:label "-f Fast-forward only (--ff-only)"
             :key "- f"
             :value false
             :incompatible [:no-ff]}
   :no-ff {:label "-n No fast-forward (--no-ff)"
           :key "- n"
           :value false
           :incompatible [:ff-only]}
   :strategy {:description "Strategy"
              :key-label "-s"
              :key "- s"
              :argument "--strategy="
              :prompt "Strategy:"
              :choices merge-strategies
              :value nil}})

(defn- merge-actions [root return-window]
  {"m" {:label "Merge"
        :fn (fn [args]
              (select-target "Merge:" (merge-targets root)
                (fn [rev] (run-merge-plain! root args rev))))}
   "e" {:label "Merge and edit message"
        :fn (fn [args]
              (select-target "Merge:" (merge-targets root)
                (fn [rev] (run-merge-editmsg! root args rev return-window))))}
   "n" {:label "Merge but don't commit"
        :fn (fn [args]
              (select-target "Merge without committing:" (merge-targets root)
                (fn [rev] (run-merge-nocommit! root args rev))))}
   "a" {:label "Absorb"
        :fn (fn [args]
              (select-target "Absorb branch:" (other-local-branches root)
                (fn [branch] (run-merge-absorb! root args branch))))}
   "p" {:label "Preview merge"
        :fn (fn [args]
              (select-target "Preview merge:" (merge-targets root)
                (fn [rev] (run-merge-preview! root args rev))))}
   "s" {:label "Squash merge"
        :fn (fn [args]
              (select-target "Squash:" (merge-targets root)
                (fn [rev] (run-merge-squash! root args rev))))}
   "d" {:label "Dissolve"
        :fn (fn [args]
              (select-target
                (str "Merge `" (or (current-branch root) "HEAD") "' into:")
                (other-local-branches root)
                (fn [branch] (run-merge-dissolve! root args branch))))}})

(defn- merge-layout []
  ["Arguments"
   {:arg :ff-only}
   {:arg :no-ff}
   {:arg :strategy}
   ""
   {:section "Actions"
    :columns 3
    :items [{:action "m"}
            {:action "e"}
            {:action "n"}
            {:action "a"}
            {:action "p"}
            {:action "s"}
            {:action "d"}]}])

(defn- merge-in-progress-actions [root return-window]
  {"m" {:label "Commit merge"
        :fn (fn [_args] (run-merge-commit! root return-window))}
   "a" {:label "Abort merge"
        :fn (fn [_args] (run-merge-abort! root))}})

(defn ^:interactive regit-merge [& [root]]
  (if-let [root (or root (project/current-project-root))]
    (let [return-window (focused-window)]
      (if (merge-in-progress? root)
        (regit-command
          {:args {}
           :return-window return-window
           :actions (merge-in-progress-actions root return-window)
           :layout [{:section "Actions"
                     :columns 3
                     :items [{:action "m"}
                             {:action "a"}]}]})
        (regit-command
          {:args (command-args)
           :return-window return-window
           :actions (merge-actions root return-window)
           :layout (merge-layout)})))
    (message "Not in a git repository")))
