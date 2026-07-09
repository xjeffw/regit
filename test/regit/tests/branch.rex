(ns regit.tests.branch
  (:require [regit.branch :as branch]
            [regit.status :as status]
            [rex.base.keys :as keys]
            [rex.base.buffer :as buffer]
            [rex.base.theme :as theme]
            [regit.command :as regit-command]
            [rex.ui.iselect :as iselect]
            [rex.ui.simple-prompt :as simple-prompt]
            [rex.string :as str]
            [rex.test :as test :refer [deftest is=]]))

(defn- git [root & args]
  (run-shell* "git" (into ["-C" root] args)))

(defn- git! [root & args]
  (let [result (apply git root args)]
    (test/assert (zero? (:code result))
      (str "git command failed: " args "\n" (:err result) (:out result)))
    result))

(defn- git-out [root & args]
  (str/trim (:out (apply git! root args))))

(defn- git-config [root key]
  (let [{:keys [code out]} (git root "config" "--get" key)]
    (when (zero? code)
      (str/trim out))))

(defn- git-local-config [root key]
  (let [{:keys [code out]} (git root "config" "--local" "--get" key)]
    (when (zero? code)
      (str/trim out))))

(defn- commit-file! [root file content subject]
  (write-file (path-join root file) content)
  (git! root "add" file)
  (git! root "commit" "-m" subject))

(defn- init-test-repo [name]
  (let [tmp-dir (temp-file-path name)
        _ (run-shell* "rm" ["-rf" tmp-dir])
        _ (run-shell* "mkdir" [tmp-dir])
        _ (git! tmp-dir "init")
        _ (git! tmp-dir "config" "user.email" "test@example.com")
        _ (git! tmp-dir "config" "user.name" "Test User")
        _ (commit-file! tmp-dir "test.txt" "hello\n" "initial")
        _ (git! tmp-dir "branch" "-M" "main")]
    tmp-dir))

(defn- cleanup [& paths]
  (doseq [path paths]
    (when path
      (run-shell* "rm" ["-rf" path]))))

(defn- branch-present? [root name]
  (let [{:keys [code]} (git root "show-ref" "--verify" "--quiet" (str "refs/heads/" name))]
    (zero? code)))

(defn- branch-rev [root branch]
  (git-out root "rev-parse" branch))

(defn- focused-buffer []
  (window-buffer (focused-window)))

(defn- command-buffer []
  (let [ui-win (minibuffer-ui-window)]
    (test/assert ui-win "regit command UI not open")
    (window-buffer ui-win)))

(defn- buffer-content [buf]
  (with-read-lock [lock (buffer-text buf)]
    (buffer/slice lock 0 (buffer/len-chars lock))))

(defn- command-text []
  (str/strip-properties (buffer-content (command-buffer))))

(defn- command-state []
  (let [ui-buf (command-buffer)]
    (binding [*buffer* ui-buf]
      @regit-command/*state*)))

(defn- command-for [key]
  (let [ui-buf (command-buffer)]
    (binding [*buffer* ui-buf]
      (regit-command/regit-command-keymap (keys/parse-key-sequence key)))))

(defn- invoke-command-key! [key]
  (let [cmd (command-for key)]
    (test/assert (ifn? cmd) (str "missing regit branch command key " key))
    (let [ui-buf (command-buffer)]
      (binding [*buffer* ui-buf]
        (cmd)))))

(defn- invoke-status-key! [key]
  (let [win (focused-window)
        buf (window-buffer win)
        cmd (keys/lookup-keymap status/regit-status-keymap (keys/parse-key-sequence key))]
    (test/assert (ifn? cmd) (str "missing regit status command key " key))
    (binding [*buffer* buf]
      (cmd))))

(defn- close-command! []
  (when-let [ui-win (minibuffer-ui-window)]
    (let [ui-buf (window-buffer ui-win)]
      (binding [*buffer* ui-buf]
        (when-let [cmd (regit-command/regit-command-keymap (keys/parse-key-sequence "q"))]
          (cmd))))))

(defn- select-current-iselect-entry! []
  (let [mb-win (minibuffer-window)]
    (test/assert mb-win "iselect minibuffer not opened")
    (binding [*buffer* (window-buffer mb-win)]
      (iselect/select-current-entry))))

(defn- set-iselect-input! [input]
  (let [mb-win (minibuffer-window)
        mb-buf (when mb-win (window-buffer mb-win))]
    (test/assert mb-buf "iselect minibuffer not opened")
    (binding [*window* mb-win
              *buffer* mb-buf]
      (set-string input mb-buf)
      (iselect/iselect-update-input))))

(defn- iselect-state []
  (let [mb-win (minibuffer-window)
        mb-buf (when mb-win (window-buffer mb-win))]
    (test/assert mb-buf "iselect minibuffer not opened")
    (binding [*window* mb-win
              *buffer* mb-buf]
      @iselect/*state*)))

(defn- invoke-iselect-key! [key]
  (let [mb-win (minibuffer-window)
        mb-buf (when mb-win (window-buffer mb-win))]
    (test/assert mb-buf "iselect minibuffer not opened")
    (binding [*window* mb-win
              *buffer* mb-buf]
      (let [cmd (keys/lookup-keymap iselect/iselect-keymap (keys/parse-key-sequence key))]
        (test/assert (ifn? cmd) (str "missing iselect key " key))
        (cmd)))))

(defn- submit-simple-prompt! [input]
  (let [mb-win (minibuffer-window)]
    (test/assert mb-win "simple-prompt minibuffer not opened")
    (binding [*buffer* (window-buffer mb-win)]
      (simple-prompt/set-input! input)
      (simple-prompt/simple-prompt-submit))))

(defn- assert-command-contains [needle]
  (let [text (command-text)]
    (test/assert (str/includes? text needle)
      (str "expected branch command to contain " needle "\nGot:\n" text))))

(defn- assert-command-green-at [needle]
  (let [buf (command-buffer)
        text (command-text)
        pos (str/index-of text needle)]
    (test/assert pos (str "missing command text " needle "\nGot:\n" text))
    (binding [*buffer* buf]
      (let [props (buffer/property-at pos)]
        (test/assert (seq props) (str "expected green properties at " needle))
        (is= (:fg (style->map (theme/color-style :green)))
          (:fg (style->map (first props))))))))

(defn- assert-command-dimmed-at [needle]
  (let [buf (command-buffer)
        text (command-text)
        pos (str/index-of text needle)]
    (test/assert pos (str "missing command text " needle "\nGot:\n" text))
    (binding [*buffer* buf]
      (let [props (buffer/property-at pos)]
        (test/assert (seq props) (str "expected dimmed properties at " needle))
        (is= (:fg (style->map (theme/style-for-face :dimmed)))
          (:fg (style->map (first props))))))))

(defn- assert-command-face-at [needle face]
  (let [buf (command-buffer)
        text (command-text)
        pos (str/index-of text needle)]
    (test/assert pos (str "missing command text " needle "\nGot:\n" text))
    (binding [*buffer* buf]
      (let [props (buffer/property-at pos)]
        (test/assert (seq props) (str "expected properties at " needle))
        (is= (:fg (style->map (theme/style-for-face face)))
          (:fg (style->map (first props))))))))

(deftest regit-branch-command-layout-and-bindings-test
  (theme/load-theme :catppuccin-frappe)
  (let [tmp-dir (init-test-repo "regit-branch-command")
        _ (git! tmp-dir "remote" "add" "origin" tmp-dir)]
    (branch/regit-branch tmp-dir)
    (let [st (command-state)]
      (test/assert st "regit-command state not initialized")
      (doseq [key ["d" "u" "r" "p" "R" "P" "B"
                   "b" "l" "c" "s" "n" "S" "o" "w" "W"
                   "f" "F" "C" "m" "x" "k" "h" "H"]]
        (test/assert (contains? (:actions st) key) (str "missing branch action " key)))
      (is= "- r" (get-in st [:args :recurse-submodules :key])))
    (doseq [needle ["Configure main"
                    "branch.main.description"
                    "branch.main.merge"
                    "branch.main.remote"
                    "branch.main.rebase"
                    "branch.main.pushRemote"
                    "Configure repository defaults"
                    "pull.rebase"
                    "remote.pushDefault"
                    "Arguments"
                    "Checkout"
                    "Create"
                    "Do"
                    "branch/revision"
                    "local branch"
                    "new spin-off"
                    "new spin-out"
                    "pull-request"
                    "from pull-request"
                    "configure..."
                    "rename"
                    "reset"
                    "delete"]]
      (assert-command-contains needle))
    (let [lines (vec (str/split (command-text) #"\n"))
          defaults-idx (first (keep-indexed
                                (fn [idx line]
                                  (when (str/includes? line "Configure repository defaults")
                                    idx))
                                lines))]
      (test/assert defaults-idx "missing Configure repository defaults section")
      (is= "" (nth lines (dec defaults-idx))))
    (assert-command-face-at "main" :regit-branch)
    (close-command!)
    (cleanup tmp-dir)))

(deftest regit-branch-configure-selects-branch-test
  (let [tmp-dir (init-test-repo "regit-branch-configure-select")]
    (branch/regit-branch tmp-dir)
    (invoke-command-key! "C")
    (select-current-iselect-entry!)
    (assert-command-contains "Configure main")
    (assert-command-contains "Configure branch creation")
    (let [lines (vec (str/split (command-text) #"\n"))
          creation-idx (first (keep-indexed
                                (fn [idx line]
                                  (when (str/includes? line "Configure branch creation")
                                    idx))
                                lines))]
      (test/assert creation-idx "missing Configure branch creation section")
      (is= "" (nth lines (dec creation-idx))))
    (close-command!)
    (cleanup tmp-dir)))

(deftest regit-branch-iselect-tab-completes-selected-entry-test
  (let [tmp-dir (init-test-repo "regit-branch-iselect-tab")]
    (git! tmp-dir "branch" "topic-alpha")
    (git! tmp-dir "branch" "topic-beta")
    (git-clear-repo-cache)
    (branch/regit-branch tmp-dir)
    (invoke-command-key! "C")
    (set-iselect-input! "alpha")
    (is= ["topic-alpha"] (:entries (iselect-state)))
    (is= 0 (:selected-idx (iselect-state)))
    (invoke-iselect-key! "<tab>")
    (is= "topic-alpha" (iselect/current-input))
    (select-current-iselect-entry!)
    (assert-command-contains "Configure topic-alpha")
    (close-command!)
    (cleanup tmp-dir)))

(deftest regit-branch-configure-cycle-options-test
  (theme/load-theme :catppuccin-frappe)
  (let [tmp-dir (init-test-repo "regit-branch-configure-cycle")
        _ (git! tmp-dir "remote" "add" "origin" tmp-dir)
        _ (git! tmp-dir "config" "pull.rebase" "true")
        _ (git! tmp-dir "config" "branch.autoSetupMerge" "always")
        _ (git! tmp-dir "config" "branch.autoSetupRebase" "always")]
    (branch/regit-branch-configure tmp-dir "main")
    (assert-command-contains "branch.main.rebase [true|false|pull.rebase:")
    (assert-command-dimmed-at "false")

    (invoke-command-key! "r")
    (is= "true" (git-config tmp-dir "branch.main.rebase"))
    (assert-command-green-at "true")
    (assert-command-dimmed-at "false")
    (invoke-command-key! "r")
    (is= "false" (git-config tmp-dir "branch.main.rebase"))
    (assert-command-green-at "false")
    (invoke-command-key! "r")
    (is= nil (git-config tmp-dir "branch.main.rebase"))

    (invoke-command-key! "p")
    (is= "origin" (git-config tmp-dir "branch.main.pushRemote"))
    (assert-command-green-at "origin")
    (invoke-command-key! "p")
    (is= nil (git-config tmp-dir "branch.main.pushRemote"))

    (invoke-command-key! "R")
    (is= "false" (git-local-config tmp-dir "pull.rebase"))
    (invoke-command-key! "R")
    (is= nil (git-local-config tmp-dir "pull.rebase"))

    (invoke-command-key! "a m")
    (is= "true" (git-local-config tmp-dir "branch.autoSetupMerge"))
    (invoke-command-key! "a m")
    (is= "false" (git-local-config tmp-dir "branch.autoSetupMerge"))
    (invoke-command-key! "a r")
    (is= "local" (git-local-config tmp-dir "branch.autoSetupRebase"))
    (invoke-command-key! "a r")
    (is= "remote" (git-local-config tmp-dir "branch.autoSetupRebase"))

    (close-command!)
    (cleanup tmp-dir)))

(deftest regit-branch-configure-description-prompt-test
  (let [tmp-dir (init-test-repo "regit-branch-configure-description")]
    (branch/regit-branch-configure tmp-dir "main")
    (invoke-command-key! "d")
    (submit-simple-prompt! "primary branch")
    (is= "primary branch" (git-config tmp-dir "branch.main.description"))
    (assert-command-contains "primary branch")
    (invoke-command-key! "d")
    (submit-simple-prompt! "")
    (is= nil (git-config tmp-dir "branch.main.description"))
    (close-command!)
    (cleanup tmp-dir)))

(deftest regit-branch-configure-upstream-toggle-test
  (let [tmp-dir (init-test-repo "regit-branch-configure-upstream")
        _ (git! tmp-dir "remote" "add" "origin" tmp-dir)
        _ (git! tmp-dir "update-ref" "refs/remotes/origin/main" "HEAD")
        _ (git! tmp-dir "branch" "--set-upstream-to=origin/main" "main")]
    (branch/regit-branch-configure tmp-dir "main")
    (assert-command-contains "branch.main.merge refs/heads/main")
    (assert-command-contains "branch.main.remote origin")
    (invoke-command-key! "u")
    (is= nil (git-config tmp-dir "branch.main.merge"))
    (is= nil (git-config tmp-dir "branch.main.remote"))
    (assert-command-contains "branch.main.merge unset")
    (close-command!)
    (cleanup tmp-dir)))

(deftest regit-branch-configure-upstream-from-status-reopens-command-test
  (let [tmp-dir (init-test-repo "regit-branch-configure-upstream-status")
        _ (git! tmp-dir "remote" "add" "origin" tmp-dir)
        _ (git! tmp-dir "update-ref" "refs/remotes/origin/main" "HEAD")]
    (status/regit-status tmp-dir)
    (invoke-status-key! "b")
    (assert-command-contains "Configure main")
    (invoke-command-key! "u")
    (set-iselect-input! "origin/main")
    (select-current-iselect-entry!)
    (is= "origin" (git-config tmp-dir "branch.main.remote"))
    (is= "refs/heads/main" (git-config tmp-dir "branch.main.merge"))
    (assert-command-contains "Configure main")
    (assert-command-contains "branch.main.merge refs/heads/main")
    (assert-command-contains "branch.main.remote origin")
    (close-command!)
    (cleanup tmp-dir)))

(deftest regit-branch-pull-request-no-refs-from-status-exits-command-test
  (let [tmp-dir (init-test-repo "regit-branch-pull-request-no-refs-status")]
    (status/regit-status tmp-dir)
    (let [status-buffer (focused-buffer)]
      (invoke-status-key! "b")
      (assert-command-contains "pull-request")
      (invoke-command-key! "f")
      (is= status-buffer (focused-buffer)))
    (cleanup tmp-dir)))

(deftest regit-branch-checkout-error-from-status-exits-iselect-test
  (let [tmp-dir (init-test-repo "regit-branch-checkout-error-status")
        worktree-dir (temp-file-path "regit-branch-checkout-error-worktree")]
    (cleanup worktree-dir)
    (git! tmp-dir "branch" "work1")
    (git! tmp-dir "worktree" "add" worktree-dir "work1")
    (git-clear-repo-cache)
    (status/regit-status tmp-dir)
    (let [status-buffer (focused-buffer)]
      (invoke-status-key! "b")
      (invoke-command-key! "b")
      (set-iselect-input! "work1")
      (select-current-iselect-entry!)
      (is= status-buffer (focused-buffer)))
    (cleanup worktree-dir tmp-dir)))

(deftest regit-branch-create-test
  (let [tmp-dir (init-test-repo "regit-branch-create")]
    (let [err (branch/create-branch! tmp-dir "feature")]
      (test/assert (not err) (str "create branch failed: " err))
      (test/assert (branch-present? tmp-dir "feature")))
    (cleanup tmp-dir)))

(deftest regit-branch-checkout-test
  (let [tmp-dir (init-test-repo "regit-branch-checkout")]
    (let [err (branch/create-branch! tmp-dir "feature")]
      (test/assert (not err) (str "create branch failed: " err))
      (let [err (branch/checkout-branch! tmp-dir "feature")]
        (test/assert (not err) (str "checkout branch failed: " err))
        (is= "feature" (branch/current-branch tmp-dir))))
    (cleanup tmp-dir)))

(deftest regit-branch-checkout-new-test
  (let [tmp-dir (init-test-repo "regit-branch-checkout-new")]
    (let [err (branch/create-and-checkout-branch! tmp-dir "feature2")]
      (test/assert (not err) (str "create+checkout branch failed: " err))
      (test/assert (branch-present? tmp-dir "feature2"))
      (is= "feature2" (branch/current-branch tmp-dir)))
    (cleanup tmp-dir)))

(deftest regit-branch-rename-test
  (let [tmp-dir (init-test-repo "regit-branch-rename")]
    (let [err (branch/rename-current-branch! tmp-dir "renamed")]
      (test/assert (not err) (str "rename branch failed: " err))
      (is= "renamed" (branch/current-branch tmp-dir)))
    (cleanup tmp-dir)))

(deftest regit-branch-delete-test
  (let [tmp-dir (init-test-repo "regit-branch-delete")]
    (let [err (branch/create-branch! tmp-dir "delete-me")]
      (test/assert (not err) (str "create branch failed: " err))
      (let [err (branch/delete-branch! tmp-dir "delete-me")]
        (test/assert (not err) (str "delete branch failed: " err))
        (test/assert (not (branch-present? tmp-dir "delete-me")))))
    (cleanup tmp-dir)))

(deftest regit-branch-create-from-remote-sets-pushremote-test
  (let [tmp-dir (init-test-repo "regit-branch-create-from-remote")]
    (git! tmp-dir "remote" "add" "fork" tmp-dir)
    (git! tmp-dir "update-ref" "refs/remotes/fork/feature" "HEAD")
    (let [err (branch/create-and-checkout-branch! tmp-dir "feature" "fork/feature" {})]
      (test/assert (not err) (str "create from remote failed: " err))
      (is= "feature" (branch/current-branch tmp-dir))
      (is= "fork" (git-config tmp-dir "branch.feature.pushRemote")))
    (cleanup tmp-dir)))

(deftest regit-branch-reset-non-current-branch-test
  (let [tmp-dir (init-test-repo "regit-branch-reset-non-current")
        old-head (branch-rev tmp-dir "HEAD")
        _ (commit-file! tmp-dir "second.txt" "second\n" "second")
        new-head (branch-rev tmp-dir "HEAD")]
    (let [err (branch/create-branch! tmp-dir "topic" old-head)]
      (test/assert (not err) (str "create topic failed: " err)))
    (let [err (branch/reset-branch! tmp-dir "topic" new-head)]
      (test/assert (not err) (str "reset branch failed: " err))
      (is= new-head (branch-rev tmp-dir "topic"))
      (is= "main" (branch/current-branch tmp-dir)))
    (cleanup tmp-dir)))

(deftest regit-branch-shelve-unshelve-test
  (let [tmp-dir (init-test-repo "regit-branch-shelve")]
    (let [err (branch/create-branch! tmp-dir "topic")]
      (test/assert (not err) (str "create topic failed: " err)))
    (let [err (branch/shelve-branch! tmp-dir "topic")]
      (test/assert (not err) (str "shelve failed: " err))
      (test/assert (not (branch-present? tmp-dir "topic"))))
    (let [shelved (first (branch/shelved-branches tmp-dir))]
      (test/assert shelved "expected shelved branch ref")
      (let [err (branch/unshelve-branch! tmp-dir shelved)]
        (test/assert (not err) (str "unshelve failed: " err))
        (test/assert (branch-present? tmp-dir "topic"))))
    (cleanup tmp-dir)))

(deftest regit-branch-worktree-branch-test
  (let [tmp-dir (init-test-repo "regit-branch-worktree")
        worktree-dir (temp-file-path "regit-branch-worktree-out")]
    (cleanup worktree-dir)
    (let [err (branch/worktree-add-branch! tmp-dir worktree-dir "topic" "HEAD")]
      (test/assert (not err) (str "worktree add branch failed: " err))
      (test/assert (path-exists? (path-join worktree-dir "test.txt"))
        "worktree should contain checked out files")
      (test/assert (branch-present? tmp-dir "topic")))
    (cleanup worktree-dir tmp-dir)))

(deftest regit-branch-pull-request-ref-test
  (let [tmp-dir (init-test-repo "regit-branch-pr-ref")]
    (git! tmp-dir "update-ref" "refs/pullreqs/42" "HEAD")
    (let [result (branch/setup-pull-request-branch! tmp-dir "pullreqs/42")]
      (test/assert (not (:error result)) (str "setup pull request branch failed: " (:error result)))
      (is= "pr-42" (:branch result))
      (test/assert (branch-present? tmp-dir "pr-42"))
      (is= "42" (git-config tmp-dir "branch.pr-42.pullRequest"))
      (is= "true" (git-config tmp-dir "branch.pr-42.rebase")))
    (cleanup tmp-dir)))
