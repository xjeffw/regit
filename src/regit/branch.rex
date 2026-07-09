(ns regit.branch
  (:require [regit.command :as regit-command :refer [regit-command]]
            [regit.util :refer [with-status-buffer-pending]]
            [rex.ui.iselect :as iselect]
            [rex.ui.simple-prompt :as simple-prompt]
            [rex.base.project :as project]
            [rex.string :as str]
            [rex.base.theme :as theme]
            [rex.base.frame :as frame])
  (:use rex.core rex.builtins))

(declare run-git! refresh-after-result! open-branch-command! regit-branch-configure)

(defn- git-cmd [root & args]
  (run-shell* "git" (into ["-C" (str root)] args)))

(defn- current-branch [root]
  (git-current-branch root))

(defn- local-branches [root]
  (vec (git-local-branches root)))

(defn- remote-branches [root]
  (vec (git-remote-branches root)))

(defn- tag-names [root]
  (vec (git-tags root)))

(def special-refnames ["HEAD" "ORIG_HEAD" "FETCH_HEAD" "MERGE_HEAD" "CHERRY_PICK_HEAD"])

(defn- rev-exists? [root rev]
  (seq (git-existing-refs root [rev])))

(defn- existing-special-refnames [root]
  (vec (git-existing-refs root special-refnames)))

(defn- branch-and-ref-targets [root]
  (vec (distinct (concat (local-branches root)
                   (remote-branches root)
                   (tag-names root)
                   (existing-special-refnames root)))))

(defn- remote-name [remote-branch]
  (first (str/split remote-branch #"/" 2)))

(defn- remote-branch-local-name [remote-branch]
  (let [[_ branch] (str/split remote-branch #"/" 2)]
    branch))

(defn- remote-branch? [root branch]
  (contains? (set (remote-branches root)) branch))

(defn- local-branch? [root branch]
  (contains? (set (local-branches root)) branch))

(defn- config-key [& parts]
  (str/join "." parts))

(defn- git-config-value [root key & [scope]]
  (if scope
    (git-config-get root key scope)
    (git-config-get root key)))

(defn- git-command-error [args result]
  (let [{:keys [code out err]} result]
    (when-not (zero? code)
      (let [details (->> [err out]
                      (map str/trim)
                      (remove str/blank?)
                      (str/join "\n"))]
        (if (str/blank? details)
          (str "git " (str/join " " args) " failed (exit " code ")")
          details)))))

(defn- git-config-set! [root key value]
  (frame/with-render-coalescing-if-needed
    (let [args ["config" key value]
          result (apply git-cmd root args)
          err (git-command-error args result)]
      (when-not err
        (git-clear-repo-cache))
      err)))

(defn- git-config-unset! [root key]
  (frame/with-render-coalescing-if-needed
    (let [args ["config" "--unset" key]
          result (apply git-cmd root args)]
      (if (= 5 (:code result))
        nil
        (let [err (git-command-error args result)]
          (when-not err
            (git-clear-repo-cache))
          err)))))

(defn- git-config-set-or-unset! [root key value]
  (if (str/blank? (or value ""))
    (git-config-unset! root key)
    (git-config-set! root key value)))

(defn- remotes [root]
  (vec (git-remotes root)))

(defn- run-git! [root & args]
  (with-status-buffer-pending root
    (let [{:keys [code out err]} (apply git-cmd root args)
          details (->> [err out]
                    (map str/trim)
                    (remove str/blank?)
                    (str/join "\n"))]
      (if (zero? code)
        (do
          (git-clear-repo-cache)
          nil)
        (if (str/blank? details)
          (str "git " (str/join " " args) " failed (exit " code ")")
          details)))))

(defn- refresh-status! [root]
  (call-var regit.status/refresh-status! root))

(defn- checkout-branch! [root branch]
  (run-git! root "checkout" branch))

(defn- checkout-args [args-values]
  (cond-> []
    (:recurse-submodules args-values) (conj "--recurse-submodules")))

(defn- checkout-revision! [root rev & [args-values]]
  (apply run-git! root "checkout" (concat (checkout-args args-values) [rev])))

(defn- create-branch! [root branch & [start-point]]
  (apply run-git! root "branch"
    (vec (remove #(str/blank? (or % "")) [branch start-point]))))

(defn- maybe-set-push-remote-for-start! [root branch start-point]
  (when (remote-branch? root start-point)
    (let [remote (remote-name start-point)
          remote-branch (remote-branch-local-name start-point)
          push-default (git-config-value root "remote.pushDefault")]
      (when (and (= branch remote-branch)
              (not= remote push-default))
        (git-config-set! root (config-key "branch" branch "pushRemote") remote)))))

(defn- create-and-checkout-branch! [root branch & [start-point args-values]]
  (let [start-point (or start-point "HEAD")
        result (apply run-git! root "checkout"
                 (concat (checkout-args args-values) ["-b" branch start-point]))]
    (when-not result
      (maybe-set-push-remote-for-start! root branch start-point))
    result))

(defn- create-orphan-branch! [root branch start-point]
  (run-git! root "checkout" "--orphan" branch start-point))

(defn- rename-branch! [root old-name new-name]
  (cond
    (str/blank? (or old-name ""))
    "Old branch name required"

    (str/blank? (or new-name ""))
    "New branch name required"

    (= old-name new-name)
    "Old and new branch names are the same"

    :else
    (let [push-remote (git-config-value root (config-key "branch" old-name "pushRemote"))
          result (run-git! root "branch" "-m" old-name new-name)]
      (when-not result
        (when (and push-remote
                (str/blank? (or (git-config-value root (config-key "branch" new-name "pushRemote")) "")))
          (git-config-set! root (config-key "branch" new-name "pushRemote") push-remote))
        (git-config-unset! root (config-key "branch" old-name "pushRemote")))
      result)))

(defn- rename-current-branch! [root new-name]
  (rename-branch! root (current-branch root) new-name))

(defn- delete-branch! [root branch]
  (run-git! root "branch" "-D" branch))

(defn- delete-remote-branch! [root remote branch]
  (run-git! root "push" remote "--delete" branch))

(defn- upstream-branch [root branch]
  (git-upstream-branch root branch))

(defn- merge-base [root left right]
  (git-merge-base root left right))

(defn- rev-parse [root rev]
  (git-rev-parse root rev))

(defn- same-rev? [root left right]
  (= (rev-parse root left) (rev-parse root right)))

(defn- worktree-dirty? [root]
  (git-worktree-dirty? root))

(defn- set-upstream! [root branch upstream]
  (if (str/blank? (or upstream ""))
    (run-git! root "branch" "--unset-upstream" branch)
    (run-git! root "branch" "--set-upstream-to" upstream branch)))

(defn- reset-branch! [root branch target]
  (cond
    (str/blank? (or branch ""))
    "No branch selected"

    (str/blank? (or target ""))
    "No reset target selected"

    (= branch (current-branch root))
    (run-git! root "reset" "--hard" target)

    :else
    (run-git! root "update-ref" "-m" (str "reset: moving to " target)
      (str "refs/heads/" branch) target)))

(defn- branch-spinoff! [root branch checkout?]
  (cond
    (str/blank? (or branch ""))
    "Branch name required"

    (local-branch? root branch)
    (str "Cannot spin off " branch ". It already exists")

    :else
    (let [current (current-branch root)
          checkout? (if (and (not checkout?) (worktree-dirty? root)) true checkout?)
          tracked (when current (upstream-branch root current))
          create-result (if current
                          (if checkout?
                            (run-git! root "checkout" "-b" branch current)
                            (run-git! root "branch" branch current))
                          (if checkout?
                            (run-git! root "checkout" "-b" branch)
                            (run-git! root "branch" branch)))]
      (when-not create-result
        (when tracked
          (set-upstream! root branch tracked)
          (when-let [base (merge-base root current tracked)]
            (when-not (same-rev? root base current)
              (if checkout?
                (run-git! root "update-ref" "-m" (str "reset: moving to " base)
                  (str "refs/heads/" current) base)
                (run-git! root "reset" "--hard" base))))))
      create-result)))

(defn- worktree-add! [root directory target]
  (run-git! root "worktree" "add" directory target))

(defn- worktree-add-branch! [root directory branch start-point]
  (run-git! root "worktree" "add" "-b" branch directory start-point))

(defn- shelved-branches [root]
  (vec (map #(subs % (count "shelved/"))
         (git-prefixed-refs root ["refs/shelved"]))))

(defn- branch-commit-date [root branch]
  (or (git-commit-date root branch)
    "unknown-date"))

(defn- shelve-branch! [root branch]
  (let [old (str "refs/heads/" branch)
        new (str "refs/shelved/" (branch-commit-date root branch) "-" branch)
        result (run-git! root "update-ref" new old)]
    (when-not result
      (git-config-unset! root (config-key "branch" branch "pushRemote"))
      (delete-branch! root branch))
    result))

(defn- unshelved-branch-name [shelved]
  (if (re-matches #"^[0-9]{4}-[0-9]{2}-[0-9]{2}-.+" shelved)
    (subs shelved 11)
    shelved))

(defn- unshelve-branch! [root shelved]
  (let [old (str "refs/shelved/" shelved)
        branch (unshelved-branch-name shelved)
        new (str "refs/heads/" branch)
        result (run-git! root "update-ref" new old)]
    (when-not result
      (run-git! root "update-ref" "-d" old))
    result))

(defn- pull-request-refs [root]
  (vec (git-prefixed-refs root ["refs/pullreqs" "refs/pull" "refs/merge-requests"])))

(defn- pull-request-number [ref]
  (second
    (or (re-matches #"^pullreqs/([0-9]+)$" ref)
      (re-matches #"^pull/([0-9]+)/(?:head|merge)$" ref)
      (re-matches #"^merge-requests/([0-9]+)/(?:head|merge)$" ref))))

(defn- setup-pull-request-branch! [root ref]
  (let [number (pull-request-number ref)
        branch (when number (str "pr-" number))]
    (cond
      (str/blank? (or ref ""))
      {:error "No pull-request selected"}

      (str/blank? (or number ""))
      {:error (str "Unsupported pull-request ref " ref)}

      (not (rev-exists? root ref))
      {:error (str "Pull-request ref " ref " does not exist")}

      :else
      (let [result (when-not (local-branch? root branch)
                     (create-branch! root branch ref))]
        (if result
          {:error result}
          (do
            (git-config-set! root (config-key "branch" branch "pullRequest") number)
            (git-config-set! root (config-key "branch" branch "rebase") "true")
            {:branch branch}))))))

(defn- pull-request-branch! [root ref checkout?]
  (let [result (setup-pull-request-branch! root ref)]
    (if-let [err (:error result)]
      err
      (if checkout?
        (refresh-after-result! root (checkout-branch! root (:branch result)))
        (refresh-after-result! root nil)))))

(defn- no-pull-request-refs! []
  (message "No pull-request refs found")
  nil)

(defn- confirmed-input? [input]
  (let [normalized (str/lower-case (str/trim (or input "")))]
    (= normalized "y")))

(defn- selected-branch [input selected]
  (let [input (str/trim (str input))
        selected (when selected (str selected))]
    (if (str/blank? (or selected ""))
      input
      selected)))

(def branch-iselect-options
  {:complete-fn #'iselect/complete-to-selection
   :sync-input-on-move? false
   :preserve-selection? true
   :clear-selection-on-input? false})

(defn- select-branch-with-entries [prompt entries-fn on-select]
  (let [entries (entries-fn nil)
        submit-fn (fn [input selected-entry]
                    (let [branch (selected-branch input selected-entry)]
                      (if (str/blank? (or branch ""))
                        (do
                          (message "No branch selected")
                          :keep-open)
                        (let [result (on-select branch)]
                          (if (string? result)
                            (fn [] (message result))
                            result)))))]
    (iselect/iselect
      prompt
      entries-fn
      (fn [_] nil)
      (assoc branch-iselect-options :submit-fn submit-fn))))

(defn- prompt-branch-name [prompt on-submit]
  (simple-prompt/simple-prompt
    {:name :regit-branch
     :prompt prompt
     :on-submit on-submit}))

(defn- confirm-delete-branch [root branch]
  (simple-prompt/simple-prompt
    {:name :regit-branch-delete
     :prompt (str "Delete branch " branch "? Type [y] to confirm:")
     :autosubmit #"."
     :on-submit (fn [input]
                  (if (confirmed-input? input)
                    (if-let [err (delete-branch! root branch)]
                      (message err)
                      (refresh-status! root))
                    (message "Regit branch delete cancelled")))}))

(defn- confirm-delete-remote-branch [root remote branch]
  (simple-prompt/simple-prompt
    {:name :regit-branch-delete
     :prompt (str "Delete remote branch " remote "/" branch "? Type [y] to confirm:")
     :autosubmit #"."
     :on-submit (fn [input]
                  (if (confirmed-input? input)
                    (if-let [err (delete-remote-branch! root remote branch)]
                      (message err)
                      (refresh-status! root))
                    (message "Regit branch delete cancelled")))}))

(defn- refresh-after-result! [root result]
  (if (string? result)
    result
    (do
      (refresh-status! root)
      nil)))

(defn- select-target [root prompt on-select]
  (let [entries (branch-and-ref-targets root)]
    (select-branch-with-entries
      prompt
      (fn [_input] entries)
      on-select)))

(defn- select-starting-point [root prompt default-start on-select]
  (let [entries (branch-and-ref-targets root)
        submit-fn (fn [input selected-entry]
                    (let [target (selected-branch input selected-entry)]
                      (if (str/blank? (or target ""))
                        (do
                          (message "No starting point selected")
                          :keep-open)
                        (let [result (on-select target)]
                          (if (string? result)
                            (fn [] (message result))
                            result)))))]
    (iselect/iselect
      prompt
      (fn [_input] entries)
      (fn [_entry] nil)
      (assoc branch-iselect-options
        :initial-input default-start
        :submit-fn submit-fn))))

(defn- prompt-branch-and-start [root prompt on-submit & [default-start]]
  (prompt-branch-name
    (str prompt " named:")
    (fn [branch]
      (if (str/blank? (or branch ""))
        (message "Branch name required")
        (select-starting-point root
          (str prompt " " branch " starting at:")
          (or default-start (current-branch root) "HEAD")
          (fn [start-point]
            (refresh-after-result! root (on-submit branch start-point))))))))

(defn- prompt-worktree-directory [root prompt branch on-submit]
  (let [default-name (str (or (path-filename root) "worktree")
                       "_"
                       (or branch "worktree"))
        default-dir (path-join (or (path-parent root) root) default-name)]
    (simple-prompt/simple-prompt
      {:name :regit-branch-worktree
       :prompt prompt
       :initial-input default-dir
       :on-submit (fn [directory]
                    (if (str/blank? (or directory ""))
                      (message "Worktree directory required")
                      (let [result (on-submit directory)]
                        (if (string? result)
                          (message result)
                          (refresh-status! root)))))})))

(defn- checkout-local-candidates [root]
  (let [current (current-branch root)
        locals (local-branches root)
        local-set (set locals)
        remote (->> (remote-branches root)
                 (remove #(contains? local-set (remote-branch-local-name %))))]
    (vec (concat (remove #(= % current) locals) remote))))

(defn- select-local-branch [root prompt on-select]
  (select-branch-with-entries prompt (fn [_input] (local-branches root)) on-select))

(defn- select-other-local-branch [root prompt on-select]
  (let [current (current-branch root)]
    (select-branch-with-entries
      prompt
      (fn [_input] (vec (remove #(= % current) (local-branches root))))
      on-select)))

(defn- configure-description! [root branch return-window reopen-fn]
  (let [key (config-key "branch" branch "description")
        current (git-config-value root key)]
    (simple-prompt/simple-prompt
      {:name :regit-branch-description
       :prompt (str "Description for " branch ":")
       :initial-input (or current "")
       :target-window return-window
       :on-submit (fn [input]
                    (when-let [err (git-config-set-or-unset! root key (str/trim input))]
                      (message err))
                    (reopen-fn))})))

(defn- configure-upstream! [root branch return-window reopen-fn]
  (let [remote (git-config-value root (config-key "branch" branch "remote"))
        merge (git-config-value root (config-key "branch" branch "merge"))]
    (if (and remote merge)
      (do
        (when-let [err (set-upstream! root branch nil)]
          (message err))
        (reopen-fn))
      (select-starting-point root
        (str "Upstream for " branch ":")
        nil
        (fn [upstream]
          (fn []
            (frame/with-render-coalescing-if-needed
              (when-let [err (set-upstream! root branch upstream)]
                (message err))
              (reopen-fn))))))))

(defn- index-of-value [values value]
  (first
    (keep-indexed
      (fn [idx candidate]
        (when (= candidate value) idx))
      values)))

(defn- next-choice-value [choices current]
  (if-let [idx (and current (index-of-value choices current))]
    (when (< idx (dec (count choices)))
      (nth choices (inc idx)))
    (first choices)))

(defn- cycle-config-choice! [root key choices]
  (let [current (git-config-value root key :local)
        next (next-choice-value choices current)]
    (git-config-set-or-unset! root key next)))

(defn- config-value-text [value]
  (if (str/blank? (or value ""))
    (theme/with-face "unset" :dimmed)
    (theme/with-color-style value :green)))

(defn- branch-config-key [branch name]
  (config-key "branch" branch name))

(defn- branch-description-label [root branch]
  (str (branch-config-key branch "description")
    " "
    (config-value-text (git-config-value root (branch-config-key branch "description")))))

(defn- branch-merge-label [root branch]
  (str (branch-config-key branch "merge")
    " "
    (config-value-text (git-config-value root (branch-config-key branch "merge")))))

(defn- branch-remote-line [root branch]
  (str "   "
    (branch-config-key branch "remote")
    " "
    (config-value-text (git-config-value root (branch-config-key branch "remote")))))

(defn- branch-rebase-label [root branch]
  (let [key (branch-config-key branch "rebase")
        local (git-config-value root key :local)
        fallback (git-config-value root "pull.rebase")
        fallback-value (or fallback "false")
        fallback-label (str "pull.rebase:" fallback-value)]
    (str key
      " "
      (regit-command/choice-bracket ["true" "false"] local fallback-label (str/blank? (or local ""))))))

(defn- branch-push-remote-label [root branch]
  (let [key (branch-config-key branch "pushRemote")
        local (git-config-value root key :local)
        fallback (git-config-value root "remote.pushDefault")]
    (str key
      " "
      (regit-command/choice-bracket (remotes root) local
        (when fallback (str "remote.pushDefault:" fallback))
        (and fallback (str/blank? (or local "")))))))

(defn- pull-rebase-label [root]
  (let [local (git-config-value root "pull.rebase" :local)
        global (git-config-value root "pull.rebase" :global)
        fallback-label (str "global:" (or global "false"))]
    (str "pull.rebase "
      (regit-command/choice-bracket ["true" "false"] local fallback-label (str/blank? (or local ""))))))

(defn- remote-push-default-label [root]
  (let [value (git-config-value root "remote.pushDefault" :local)
        global (git-config-value root "remote.pushDefault" :global)]
    (str "remote.pushDefault "
      (regit-command/choice-bracket (remotes root) value
        (when global (str "global:" global))
        (and global (str/blank? (or value "")))))))

(defn- branch-auto-setup-merge-label [root]
  (let [value (git-config-value root "branch.autoSetupMerge" :local)
        fallback-label "default:true"]
    (str "branch.autoSetupMerge "
      (regit-command/choice-bracket ["always" "true" "false"] value fallback-label (str/blank? (or value ""))))))

(defn- branch-auto-setup-rebase-label [root]
  (let [value (git-config-value root "branch.autoSetupRebase" :local)
        fallback-label "default:never"]
    (str "branch.autoSetupRebase "
      (regit-command/choice-bracket ["always" "local" "remote" "never"] value fallback-label (str/blank? (or value ""))))))

(defn- cycle-and-reopen! [root key choices reopen-fn]
  (frame/with-render-coalescing-if-needed
    (when-let [err (cycle-config-choice! root key choices)]
      (message err))
    (reopen-fn)))

(defn- cycle-remote-choice-and-reopen! [root key reopen-fn]
  (let [choices (remotes root)]
    (if (seq choices)
      (cycle-and-reopen! root key choices reopen-fn)
      (do
        (message "No remotes configured")
        (reopen-fn)))))

(defn- update-default-branch! [root]
  (if-let [remote (first (remotes root))]
    (refresh-after-result! root (run-git! root "remote" "set-head" "--auto" remote))
    "No remote configured"))

(defn- branch-config-actions [root branch return-window reopen-fn]
  {"d" {:label (branch-description-label root branch)
        :fn (fn [_args] (configure-description! root branch return-window reopen-fn))}
   "u" {:label (branch-merge-label root branch)
        :fn (fn [_args] (configure-upstream! root branch return-window reopen-fn))}
   "r" {:label (branch-rebase-label root branch)
        :fn (fn [_args]
              (cycle-and-reopen! root (branch-config-key branch "rebase") ["true" "false"] reopen-fn))}
   "p" {:label (branch-push-remote-label root branch)
        :fn (fn [_args]
              (cycle-remote-choice-and-reopen! root (branch-config-key branch "pushRemote") reopen-fn))}})

(defn- repo-config-actions [root reopen-fn]
  {"R" {:label (pull-rebase-label root)
        :fn (fn [_args] (cycle-and-reopen! root "pull.rebase" ["true" "false"] reopen-fn))}
   "P" {:label (remote-push-default-label root)
        :fn (fn [_args] (cycle-remote-choice-and-reopen! root "remote.pushDefault" reopen-fn))}
   "B" {:label "Update default branch"
        :fn (fn [_args] (update-default-branch! root))}})

(defn- branch-creation-config-actions [root reopen-fn]
  {"a m" {:label (branch-auto-setup-merge-label root)
          :fn (fn [_args]
                (cycle-and-reopen! root "branch.autoSetupMerge" ["always" "true" "false"] reopen-fn))}
   "a r" {:label (branch-auto-setup-rebase-label root)
          :fn (fn [_args]
                (cycle-and-reopen! root "branch.autoSetupRebase" ["always" "local" "remote" "never"] reopen-fn))}})

(defn- branch-config-heading [branch]
  (str
    (theme/with-face "Configure " :regit-header)
    (theme/with-face branch :regit-branch)))

(defn- branch-config-layout [root branch]
  [{:section (branch-config-heading branch)
    :items [{:action "d"}
            {:action "u"}
            (branch-remote-line root branch)
            {:action "r"}
            {:action "p"}]}])

(defn- repo-config-layout []
  [{:section "Configure repository defaults"
    :items [{:action "R"}
            {:action "P"}
            {:action "B"}]}])

(defn- branch-creation-config-layout []
  [{:section "Configure branch creation"
    :items [{:action "a m"}
            {:action "a r"}]}])

(defn- branch-arguments []
  {:recurse-submodules {:key "- r"
                        :label "-r Recurse submodules when checking out an existing branch"
                        :value false}})

(defn- branch-action-layout []
  [{:horizontal-sections
    [{:section "Checkout"
      :items [{:action "b"}
              {:action "l"}]}
     {:section ""
      :items [{:action "c"}
              {:action "s"}
              {:action "f"}]}
     {:section "Create"
      :items [{:action "n"}
              {:action "S"}
              {:action "F"}]}
     {:section "Do"
      :items [{:action "C"}
              {:action "m"}
              {:action "x"}
              {:action "k"}]}]
    :gap "     "}])

(defn- checkout-local-or-new! [root target args-values]
  (let [remote-set (set (remote-branches root))]
    (cond
      (contains? remote-set target)
      (create-and-checkout-branch! root (remote-branch-local-name target) target args-values)

      (local-branch? root target)
      (checkout-revision! root target args-values)

      :else
      (fn []
        (select-starting-point root
          (str "Create and checkout " target " starting at:")
          (or (current-branch root) "HEAD")
          (fn [start-point]
            (refresh-after-result! root
              (create-and-checkout-branch! root target start-point args-values))))))))

(defn- prompt-worktree-checkout [root]
  (select-target root "In new worktree; checkout:"
    (fn [target]
      (fn []
        (prompt-worktree-directory root
          (str "Checkout " target " in new worktree:")
          target
          (fn [directory]
            (worktree-add! root directory target)))))))

(defn- prompt-worktree-branch [root]
  (prompt-branch-and-start root "In new worktree; checkout new branch"
    (fn [branch start-point]
      (fn []
        (prompt-worktree-directory root
          (str "Checkout " branch " in new worktree:")
          branch
          (fn [directory]
            (worktree-add-branch! root directory branch start-point)))))))

(defn- branch-command-actions [root return-window]
  (let [current (current-branch root)]
    {"b" {:label "branch/revision"
          :fn (fn [args]
                (select-target root "Checkout branch or revision:"
                  (fn [target]
                    (refresh-after-result! root (checkout-revision! root target args)))))}
     "l" {:label "local branch"
          :fn (fn [args]
                (select-branch-with-entries
                  "Checkout local branch:"
                  (fn [_input] (checkout-local-candidates root))
                  (fn [target]
                    (let [result (checkout-local-or-new! root target args)]
                      (if (ifn? result)
                        result
                        (refresh-after-result! root result))))))}
     "c" {:label "new branch"
          :fn (fn [args]
                (prompt-branch-and-start root "Create and checkout branch"
                  (fn [branch start-point]
                    (create-and-checkout-branch! root branch start-point args))))}
     "s" {:label "new spin-off"
          :fn (fn [_args]
                (prompt-branch-name "Spin off branch:"
                  (fn [branch]
                    (let [result (branch-spinoff! root branch true)]
                      (if (string? result)
                        (message result)
                        (refresh-status! root))))))}
     "n" {:label "new branch"
          :fn (fn [_args]
                (prompt-branch-and-start root "Create branch"
                  (fn [branch start-point]
                    (create-branch! root branch start-point))))}
     "S" {:label "new spin-out"
          :fn (fn [_args]
                (prompt-branch-name "Spin out branch:"
                  (fn [branch]
                    (let [result (branch-spinoff! root branch false)]
                      (if (string? result)
                        (message result)
                        (refresh-status! root))))))}
     "o" {:label "new orphan"
          :fn (fn [_args]
                (prompt-branch-and-start root "Create and checkout orphan branch"
                  (fn [branch start-point]
                    (create-orphan-branch! root branch start-point))))}
     "w" {:label "new worktree"
          :fn (fn [_args] (prompt-worktree-checkout root))}
     "W" {:label "new worktree"
          :fn (fn [_args] (prompt-worktree-branch root))}
     "f" {:label "pull-request"
          :fn (fn [_args]
                (let [refs (pull-request-refs root)]
                  (if (seq refs)
                    (select-branch-with-entries
                      "Checkout pull request:"
                      (fn [_input] refs)
                      (fn [ref]
                        (pull-request-branch! root ref true)))
                    (no-pull-request-refs!))))}
     "F" {:label "from pull-request"
          :fn (fn [_args]
                (let [refs (pull-request-refs root)]
                  (if (seq refs)
                    (select-branch-with-entries
                      "Branch pull request:"
                      (fn [_input] refs)
                      (fn [ref]
                        (pull-request-branch! root ref false)))
                    (no-pull-request-refs!))))}
     "C" {:label "configure..."
          :fn (fn [_args]
                (select-local-branch root "Configure branch:"
                  (fn [branch]
                    (fn []
                      (regit-branch-configure root branch return-window)))))}
     "m" {:label "rename"
          :fn (fn [_args]
                (select-local-branch root "Rename branch:"
                  (fn [branch]
                    (fn []
                      (prompt-branch-name (str "Rename branch " branch " to:")
                        (fn [new-name]
                          (let [result (rename-branch! root branch new-name)]
                            (if (string? result)
                              (message result)
                              (refresh-status! root)))))))))}
     "x" {:label "reset"
          :fn (fn [_args]
                (select-local-branch root "Reset branch:"
                  (fn [branch]
                    (fn []
                      (select-starting-point root
                        (str "Reset " branch " to:")
                        (or (upstream-branch root branch) branch)
                        (fn [target]
                          (refresh-after-result! root (reset-branch! root branch target))))))))}
     "k" {:label "delete"
          :fn (fn [_args]
                (let [locals (local-branches root)
                      remotes (remote-branches root)
                      remote-set (set remotes)
                      entries (vec (distinct (concat locals remotes)))
                      delete-target (fn [target]
                                      (if (contains? remote-set target)
                                        (let [[remote & rest] (str/split target #"/")
                                              branch (str/join "/" rest)]
                                          (if (str/blank? branch)
                                            "Invalid remote branch"
                                            (fn []
                                              (confirm-delete-remote-branch root remote branch))))
                                        (let [current (current-branch root)]
                                          (if (= target current)
                                            "Cannot delete current branch"
                                            (fn []
                                              (confirm-delete-branch root target))))))]
                  (select-branch-with-entries
                    "Delete branch:"
                    (fn [_input] entries)
                    delete-target)))}
     "h" {:label "shelve"
          :fn (fn [_args]
                (select-other-local-branch root "Shelve branch:"
                  (fn [branch]
                    (refresh-after-result! root (shelve-branch! root branch)))))}
     "H" {:label "unshelve"
          :fn (fn [_args]
                (select-branch-with-entries
                  "Unshelve branch:"
                  (fn [_input] (shelved-branches root))
                  (fn [branch]
                    (refresh-after-result! root (unshelve-branch! root branch)))))}}))

(defn- branch-command-layout [root current]
  (vec (concat
         (when current
           (concat
             (branch-config-layout root current)
             [""]))
         (repo-config-layout)
         [""
          "Arguments"
          {:arg :recurse-submodules}
          ""]
         (branch-action-layout))))

(defn open-branch-command! [root return-window]
  (let [root (str root)
        current (current-branch root)
        reopen-fn (fn [] (open-branch-command! root return-window))
        config-actions (if current
                         (branch-config-actions root current return-window reopen-fn)
                         {})
        repo-actions (repo-config-actions root reopen-fn)]
    (regit-command
      {:args (branch-arguments)
       :return-window return-window
       :actions (merge
                  config-actions
                  repo-actions
                  (branch-command-actions root return-window))
       :layout (branch-command-layout root current)})))

(defn regit-branch-configure
  ([root branch]
   (regit-branch-configure root branch (focused-window)))
  ([root branch return-window]
   (let [root (str root)
         reopen-fn (fn [] (regit-branch-configure root branch return-window))]
     (regit-command
       {:args {}
        :return-window return-window
        :actions (merge
                   (branch-config-actions root branch return-window reopen-fn)
                   (repo-config-actions root reopen-fn)
                   (branch-creation-config-actions root reopen-fn))
        :layout (vec (concat
                       (branch-config-layout root branch)
                       [""]
                       (repo-config-layout)
                       [""]
                       (branch-creation-config-layout)))}))))

(defn ^:interactive regit-branch [& [root]]
  (if-let [root (or root (project/current-project-root))]
    (open-branch-command! root (focused-window))
    (message "Not in a git repository")))
