(ns regit.tests.merge
  (:require [regit.merge :as merge]
            [regit.status :as status]
            [regit.tests.util :refer [assert-focused-command
                                      assert-focused-regit-commit
                                      assert-focused-status
                                      branch-present?
                                      buffer-content
                                      cleanup
                                      command-state
                                      current-branch
                                      focused-buffer
                                      focused-buffer-name
                                      focused-content
                                      git
                                      git!
                                      git-out
                                      head-subject
                                      messages-content
                                      minibuffer-ui-content
                                      send-keys
                                      status-content
                                      wait-for-focused-status
                                      wait-for-message]]
            [regit.command :as regit-command]
            [rex.base.buffer :as buffer]
            [rex.base.frame :as frame]
            [rex.base.keys :as keys]
            [rex.ui.iselect :as iselect]
            [rex.string :as str]
            [rex.test :as test :refer [deftest is= is-error]]))

(def empty-merge-args {:ff-only false :no-ff false :strategy nil})

(defn- init-merge-test-repo [name]
  (let [tmp-dir (temp-file-path name)
        _ (run-shell* "rm" ["-rf" tmp-dir] {:direnv false})
        _ (run-shell* "mkdir" [tmp-dir] {:direnv false})
        _ (git! tmp-dir "init")
        _ (git! tmp-dir "config" "user.email" "test@example.com")
        _ (git! tmp-dir "config" "user.name" "Test User")
        base-path (path-join tmp-dir "base.txt")
        _ (write-file base-path "base\n")
        _ (git! tmp-dir "add" "base.txt")
        _ (git! tmp-dir "commit" "-m" "initial")
        _ (git! tmp-dir "branch" "-M" "main")
        _ (git! tmp-dir "checkout" "-b" "feature")
        feature-path (path-join tmp-dir "feature.txt")
        _ (write-file feature-path "feature\n")
        _ (git! tmp-dir "add" "feature.txt")
        _ (git! tmp-dir "commit" "-m" "feature")
        _ (git! tmp-dir "checkout" "main")
        main-path (path-join tmp-dir "main.txt")
        _ (write-file main-path "main\n")
        _ (git! tmp-dir "add" "main.txt")
        _ (git! tmp-dir "commit" "-m" "main")]
    tmp-dir))

(defn- init-conflicted-merge-test-repo [name]
  (let [tmp-dir (temp-file-path name)
        _ (run-shell* "rm" ["-rf" tmp-dir] {:direnv false})
        _ (run-shell* "mkdir" [tmp-dir] {:direnv false})
        _ (git! tmp-dir "init")
        _ (git! tmp-dir "config" "user.email" "test@example.com")
        _ (git! tmp-dir "config" "user.name" "Test User")
        conflict-path (path-join tmp-dir "conflict.txt")
        _ (write-file conflict-path "base\n")
        _ (git! tmp-dir "add" "conflict.txt")
        _ (git! tmp-dir "commit" "-m" "initial")
        _ (git! tmp-dir "branch" "-M" "main")
        _ (git! tmp-dir "checkout" "-b" "feature")
        _ (write-file conflict-path "theirs\n")
        _ (git! tmp-dir "commit" "-am" "feature conflict")
        _ (git! tmp-dir "checkout" "main")
        _ (write-file conflict-path "ours\n")
        _ (git! tmp-dir "commit" "-am" "main conflict")
        merge-result (git tmp-dir "merge" "feature")]
    (test/assert (not (zero? (:code merge-result))) "merge should stop with a conflict")
    (test/assert (path-exists? (path-join tmp-dir ".git" "MERGE_HEAD")) "MERGE_HEAD should exist after conflicted merge")
    tmp-dir))

(defn- merge-head? [root]
  (path-exists? (path-join root ".git" "MERGE_HEAD")))

(defn- staged-file? [root file]
  (str/includes? (git-out root "diff" "--cached" "--name-only") file))

(defn- assert-focused-merge-preview [context]
  (test/assert (str/includes? (focused-buffer-name) "regit-merge-preview")
    (str context ": expected merge preview buffer, got " (focused-buffer-name)))
  (test/assert (= true (:read-only @(buffer-state (focused-buffer))))
    (str context ": merge preview buffer should be read-only")))

(defn- open-merge-command-from-status! [root]
  (status/regit-status root)
  (assert-focused-status root "before opening merge")
  (send-keys "m")
  (assert-focused-command "Merge and edit message" "after pressing m"))

(defn- choose-first-target! []
  (send-keys "<enter>"))

(defn- choose-ours-strategy! []
  (send-keys "<down> <down> <down> <enter>"))

(deftest regit-merge-args-test
  (is= ["--ff-only" "--strategy=ours"]
    (merge/merge-args {:ff-only true :no-ff false :strategy "ours"}))
  (is= ["--no-ff"]
    (merge/merge-args {:ff-only false :no-ff true :strategy nil})))

(deftest regit-merge-ui-test
  (let [tmp-dir (init-merge-test-repo "regit-merge-ui")]
    (status/regit-status tmp-dir)
    (let [status-key (keys/lookup-keymap status/regit-status-keymap (keys/parse-key-sequence "m"))]
      (test/assert (ifn? status-key) "regit-status did not bind m to merge"))
    (merge/regit-merge tmp-dir)
    (let [ui-win (minibuffer-ui-window)]
      (test/assert ui-win "minibuffer-ui-window not opened")
      (let [ui-buf (window-buffer ui-win)]
        (binding [*buffer* ui-buf
                  *window* ui-win
                  *mode* :regit-command
                  *submodes* #{}]
          (let [text (with-read-lock [lock (buffer-text)]
                       (buffer/slice lock 0 (buffer/len-chars lock)))
                st @regit-command/*state*]
            (is= "- f" (get-in st [:args :ff-only :key]))
            (is= "- n" (get-in st [:args :no-ff :key]))
            (is= "- s" (get-in st [:args :strategy :key]))
            (test/assert (contains? (:actions st) "m"))
            (test/assert (contains? (:actions st) "e"))
            (test/assert (contains? (:actions st) "n"))
            (test/assert (contains? (:actions st) "a"))
            (test/assert (contains? (:actions st) "p"))
            (test/assert (contains? (:actions st) "s"))
            (test/assert (contains? (:actions st) "d"))
            (test/assert (str/includes? text "Fast-forward only"))
            (test/assert (str/includes? text "--ff-only"))
            (test/assert (str/includes? text "No fast-forward"))
            (test/assert (str/includes? text "--no-ff"))
            (test/assert (str/includes? text "Strategy"))
            (test/assert (str/includes? text "--strategy="))
            (test/assert (str/includes? text "Merge and edit message"))
            (test/assert (str/includes? text "Merge but don't commit"))
            (test/assert (str/includes? text "Preview merge"))
            (test/assert (str/includes? text "Squash merge"))
            (test/assert (str/includes? text "Dissolve")))

          (let [ff-cmd (regit-command/regit-command-keymap (keys/parse-key-sequence "- f"))
                no-ff-cmd (regit-command/regit-command-keymap (keys/parse-key-sequence "- n"))
                strategy-cmd (regit-command/regit-command-keymap (keys/parse-key-sequence "- s"))
                close-cmd (regit-command/regit-command-keymap (keys/parse-key-sequence "q"))]
            (test/assert (ifn? ff-cmd) "could not find --ff-only command")
            (test/assert (ifn? no-ff-cmd) "could not find --no-ff command")
            (test/assert (ifn? strategy-cmd) "could not find --strategy command")
            (keys/run-key-command ff-cmd (keys/parse-key-sequence "- f"))
            (test/assert (get-in @regit-command/*state* [:args :ff-only :value]))
            (test/assert (not (get-in @regit-command/*state* [:args :no-ff :value])))
            (keys/run-key-command no-ff-cmd (keys/parse-key-sequence "- n"))
            (test/assert (get-in @regit-command/*state* [:args :no-ff :value]))
            (test/assert (not (get-in @regit-command/*state* [:args :ff-only :value])))
            (keys/run-key-command strategy-cmd (keys/parse-key-sequence "- s"))
            (let [prompt-win (minibuffer-window)]
              (test/assert prompt-win "iselect minibuffer not opened for strategy")
              (binding [*buffer* (window-buffer prompt-win)
                        *window* prompt-win
                        *mode* :iselect
                        *submodes* #{}]
                (iselect/select-current-entry)))
            (is= "resolve" (get-in @regit-command/*state* [:args :strategy :value]))
            (when (ifn? close-cmd)
              (close-cmd))))))
    (cleanup tmp-dir)))

(deftest regit-merge-plain-test
  (let [tmp-dir (init-merge-test-repo "regit-merge-plain")]
    (let [err (merge/run-merge-plain! tmp-dir empty-merge-args "feature")]
      (test/assert (not err) (str "merge failed: " err))
      (is= "main" (current-branch tmp-dir))
      (test/assert (path-exists? (path-join tmp-dir "feature.txt")))
      (test/assert (str/includes? (head-subject tmp-dir) "Merge")))
    (cleanup tmp-dir)))

(deftest regit-merge-ff-only-failure-test
  (let [tmp-dir (init-merge-test-repo "regit-merge-ff-only-failure")]
    (let [err (merge/run-merge-plain! tmp-dir {:ff-only true :no-ff false :strategy nil} "feature")]
      (test/assert (string? err) "ff-only divergent merge should fail")
      (is= "main" (current-branch tmp-dir))
      (test/assert (not (path-exists? (path-join tmp-dir "feature.txt")))))
    (cleanup tmp-dir)))

(deftest regit-merge-strategy-test
  (let [tmp-dir (init-merge-test-repo "regit-merge-strategy")]
    (let [err (merge/run-merge-plain! tmp-dir {:ff-only false :no-ff true :strategy "ours"} "feature")]
      (test/assert (not err) (str "merge with ours strategy failed: " err))
      (test/assert (str/includes? (head-subject tmp-dir) "Merge"))
      (test/assert (not (path-exists? (path-join tmp-dir "feature.txt")))))
    (cleanup tmp-dir)))

(deftest regit-merge-nocommit-test
  (let [tmp-dir (init-merge-test-repo "regit-merge-nocommit")
        original-head (head-subject tmp-dir)]
    (let [err (merge/run-merge-nocommit! tmp-dir empty-merge-args "feature")]
      (test/assert (not err) (str "no-commit merge failed: " err))
      (test/assert (merge-head? tmp-dir) "MERGE_HEAD should exist")
      (test/assert (staged-file? tmp-dir "feature.txt") "feature.txt should be staged")
      (is= original-head (head-subject tmp-dir)))
    (cleanup tmp-dir)))

(deftest regit-merge-edit-message-test
  (let [tmp-dir (init-merge-test-repo "regit-merge-edit-message")
        return-window (focused-window)]
    (let [err (merge/run-merge-editmsg! tmp-dir empty-merge-args "feature" return-window)]
      (test/assert (not err) (str "edit-message merge failed: " err))
      (assert-focused-regit-commit "edit-message merge")
      (set-string "custom merge message\n" (focused-buffer))
      (send-keys "C-c C-c")
      (test/wait-for [state {:subject (head-subject tmp-dir)
                             :merge-head? (merge-head? tmp-dir)}]
        :until (and (= "custom merge message" (:subject state)) (not (:merge-head? state)))
        :timeout-message "merge editor submit did not create custom merge commit and clear MERGE_HEAD"))
    (cleanup tmp-dir)))

(deftest regit-merge-squash-test
  (let [tmp-dir (init-merge-test-repo "regit-merge-squash")
        original-head (head-subject tmp-dir)]
    (let [err (merge/run-merge-squash! tmp-dir empty-merge-args "feature")]
      (test/assert (not err) (str "squash merge failed: " err))
      (test/assert (not (merge-head? tmp-dir)) "squash merge should not create MERGE_HEAD")
      (test/assert (staged-file? tmp-dir "feature.txt") "feature.txt should be staged")
      (is= original-head (head-subject tmp-dir)))
    (cleanup tmp-dir)))

(deftest regit-merge-absorb-test
  (let [tmp-dir (init-merge-test-repo "regit-merge-absorb")]
    (let [err (merge/run-merge-absorb! tmp-dir empty-merge-args "feature")]
      (test/assert (not err) (str "absorb failed: " err))
      (test/assert (path-exists? (path-join tmp-dir "feature.txt")))
      (test/assert (not (branch-present? tmp-dir "feature")) "feature branch should be deleted"))
    (cleanup tmp-dir)))

(deftest regit-merge-dissolve-test
  (let [tmp-dir (init-merge-test-repo "regit-merge-dissolve")]
    (git! tmp-dir "checkout" "feature")
    (let [err (merge/run-merge-dissolve! tmp-dir empty-merge-args "main")]
      (test/assert (not err) (str "dissolve failed: " err))
      (is= "main" (current-branch tmp-dir))
      (test/assert (path-exists? (path-join tmp-dir "feature.txt")))
      (test/assert (not (branch-present? tmp-dir "feature")) "feature branch should be deleted"))
    (cleanup tmp-dir)))

(deftest regit-merge-preview-test
  (let [tmp-dir (init-merge-test-repo "regit-merge-preview")]
    (let [err (merge/run-merge-preview! tmp-dir empty-merge-args "feature")]
      (test/assert (not err) (str "preview failed: " err))
      (let [preview-window (focused-window)
            preview-buffer (window-buffer preview-window)]
        (binding [*buffer* preview-buffer
                  *window* preview-window
                  *mode* :regit-merge-preview
                  *submodes* #{}]
          (is= :regit-merge-preview *mode*)
          (let [text (with-read-lock [lock (buffer-text)]
                       (buffer/slice lock 0 (buffer/len-chars lock)))]
            (test/assert (str/includes? text "feature.txt") "preview should mention feature.txt")))))
    (cleanup tmp-dir)))

(deftest regit-merge-abort-test
  (let [tmp-dir (init-merge-test-repo "regit-merge-abort")]
    (let [err (merge/run-merge-nocommit! tmp-dir empty-merge-args "feature")]
      (test/assert (not err) (str "no-commit merge failed: " err))
      (test/assert (merge-head? tmp-dir) "MERGE_HEAD should exist before abort"))
    (let [err (merge/run-merge-abort! tmp-dir)]
      (test/assert (not err) (str "abort failed: " err))
      (test/assert (not (merge-head? tmp-dir)) "MERGE_HEAD should be gone after abort")
      (test/assert (not (path-exists? (path-join tmp-dir "feature.txt")))))
    (cleanup tmp-dir)))

(deftest regit-status-shows-in-progress-merge-test
  (let [tmp-dir (init-merge-test-repo "regit-status-merge-in-progress")]
    (let [err (merge/run-merge-nocommit! tmp-dir empty-merge-args "feature")]
      (test/assert (not err) (str "no-commit merge failed: " err)))
    (status/regit-status tmp-dir)
    (let [content (focused-content)]
      (test/assert (str/includes? content "Merging feature:") "status should show merge heading")
      (test/assert (str/includes? content "feature") "status should show merged commit")
      (test/assert (str/includes? content "Staged changes") "status should show staged merge result"))
    (merge/run-merge-abort! tmp-dir)
    (cleanup tmp-dir)))

(deftest regit-status-shows-conflicted-in-progress-merge-test
  (let [tmp-dir (init-conflicted-merge-test-repo "regit-status-conflicted-merge")]
    (status/regit-status tmp-dir)
    (let [content (focused-content)]
      (test/assert (str/includes? content "Merging feature:") "status should show merge heading")
      (test/assert (str/includes? content "feature conflict") "status should show merged commit")
      (test/assert (str/includes? content "Unstaged changes") "status should show unstaged conflict section")
      (test/assert (str/includes? content "Staged changes") "status should show staged conflict section")
      (test/assert (str/includes? content "both modified conflict.txt")
        (str "status should show Magit-style conflict state. Got: " content)))
    (merge/run-merge-abort! tmp-dir)
    (cleanup tmp-dir)))

(deftest regit-merge-key-flow-plain-merge-test
  (let [tmp-dir (init-merge-test-repo "regit-merge-key-plain")]
    (open-merge-command-from-status! tmp-dir)
    (send-keys "m")
    (choose-first-target!)
    (assert-focused-status tmp-dir "plain merge complete")
    (test/assert (path-exists? (path-join tmp-dir "feature.txt")) "plain merge should add feature file")
    (test/assert (str/includes? (head-subject tmp-dir) "Merge") "plain merge should create merge commit")
    (cleanup tmp-dir)))

(deftest regit-merge-key-flow-edit-message-test
  (let [tmp-dir (init-merge-test-repo "regit-merge-key-edit-message")]
    (open-merge-command-from-status! tmp-dir)
    (send-keys "e")
    (choose-first-target!)
    (assert-focused-regit-commit "edit-message merge")
    (send-keys "C-c C-c")
    (test/wait-for [state {:merge-head? (merge-head? tmp-dir)
                           :subject (head-subject tmp-dir)}]
      :until (and (not (:merge-head? state)) (str/includes? (:subject state) "Merge"))
      :timeout-message "edit-message merge should create merge commit and clear MERGE_HEAD")
    (wait-for-focused-status tmp-dir "edit-message merge committed")
    (cleanup tmp-dir)))

(deftest regit-merge-key-flow-edit-message-editor-abort-test
  (let [tmp-dir (init-merge-test-repo "regit-merge-key-edit-message-editor-abort")]
    (open-merge-command-from-status! tmp-dir)
    (send-keys "e")
    (choose-first-target!)
    (assert-focused-regit-commit "edit-message merge before editor abort")
    (send-keys "C-c C-k")
    (wait-for-message "Aborted merge commit message edit" "edit-message merge editor abort")
    (wait-for-focused-status tmp-dir "edit-message merge editor aborted")
    (test/assert (merge-head? tmp-dir) "aborted editor should leave merge in progress")
    (merge/run-merge-abort! tmp-dir)
    (cleanup tmp-dir)))

(deftest regit-merge-key-flow-nocommit-then-commit-test
  (let [tmp-dir (init-merge-test-repo "regit-merge-key-nocommit-commit")]
    (open-merge-command-from-status! tmp-dir)
    (send-keys "n")
    (choose-first-target!)
    (assert-focused-status tmp-dir "no-commit merge complete")
    (test/assert (merge-head? tmp-dir) "no-commit merge should leave merge in progress")
    (let [content (focused-content)]
      (test/assert (str/includes? content "Merging feature:") "status should show in-progress merge")
      (test/assert (str/includes? content "feature.txt") "status should show staged feature file"))
    (send-keys "m")
    (assert-focused-command "Commit merge" "in-progress merge command")
    (test/assert (str/includes? (minibuffer-ui-content) "Abort merge") "in-progress command should include abort")
    (send-keys "m")
    (assert-focused-regit-commit "commit in-progress merge")
    (send-keys "C-c C-c")
    (test/wait-for [state {:merge-head? (merge-head? tmp-dir)
                           :subject (head-subject tmp-dir)}]
      :until (and (not (:merge-head? state)) (str/includes? (:subject state) "Merge"))
      :timeout-message "merge commit should clear MERGE_HEAD and create merge commit")
    (wait-for-focused-status tmp-dir "in-progress merge committed")
    (cleanup tmp-dir)))

(deftest regit-merge-key-flow-nocommit-then-commit-editor-abort-test
  (let [tmp-dir (init-merge-test-repo "regit-merge-key-nocommit-commit-editor-abort")]
    (open-merge-command-from-status! tmp-dir)
    (send-keys "n")
    (choose-first-target!)
    (assert-focused-status tmp-dir "no-commit merge before editor abort")
    (test/assert (merge-head? tmp-dir) "no-commit merge should leave merge in progress")
    (send-keys "m")
    (assert-focused-command "Commit merge" "in-progress merge command before editor abort")
    (send-keys "m")
    (assert-focused-regit-commit "commit in-progress merge before editor abort")
    (send-keys "C-c C-k")
    (wait-for-message "Aborted merge commit message edit" "in-progress merge editor abort")
    (wait-for-focused-status tmp-dir "in-progress merge editor aborted")
    (test/assert (merge-head? tmp-dir) "aborted editor should leave merge in progress")
    (merge/run-merge-abort! tmp-dir)
    (cleanup tmp-dir)))

(deftest regit-merge-key-flow-nocommit-then-abort-test
  (let [tmp-dir (init-merge-test-repo "regit-merge-key-nocommit-abort")]
    (open-merge-command-from-status! tmp-dir)
    (send-keys "n")
    (choose-first-target!)
    (assert-focused-status tmp-dir "no-commit merge before abort")
    (test/assert (merge-head? tmp-dir) "no-commit merge should leave merge in progress")
    (send-keys "m")
    (assert-focused-command "Abort merge" "in-progress abort command")
    (send-keys "a")
    (assert-focused-status tmp-dir "merge abort complete")
    (test/assert (not (merge-head? tmp-dir)) "abort should clear MERGE_HEAD")
    (test/assert (not (path-exists? (path-join tmp-dir "feature.txt"))) "abort should remove merge result from worktree")
    (cleanup tmp-dir)))

(deftest regit-merge-key-flow-squash-test
  (let [tmp-dir (init-merge-test-repo "regit-merge-key-squash")
        original-head (head-subject tmp-dir)]
    (open-merge-command-from-status! tmp-dir)
    (send-keys "s")
    (choose-first-target!)
    (assert-focused-status tmp-dir "squash merge complete")
    (test/assert (not (merge-head? tmp-dir)) "squash merge should not create MERGE_HEAD")
    (test/assert (staged-file? tmp-dir "feature.txt") "squash merge should stage feature file")
    (is= original-head (head-subject tmp-dir))
    (cleanup tmp-dir)))

(deftest regit-merge-key-flow-preview-test
  (let [tmp-dir (init-merge-test-repo "regit-merge-key-preview")]
    (open-merge-command-from-status! tmp-dir)
    (send-keys "p")
    (choose-first-target!)
    (assert-focused-merge-preview "preview merge")
    (test/assert (str/includes? (focused-content) "feature.txt") "preview should show feature file")
    (send-keys "q")
    (assert-focused-status tmp-dir "preview merge closed")
    (cleanup tmp-dir)))

(deftest regit-merge-key-flow-absorb-test
  (let [tmp-dir (init-merge-test-repo "regit-merge-key-absorb")]
    (open-merge-command-from-status! tmp-dir)
    (send-keys "a")
    (choose-first-target!)
    (assert-focused-status tmp-dir "absorb complete")
    (test/assert (path-exists? (path-join tmp-dir "feature.txt")) "absorb should merge feature file")
    (test/assert (not (branch-present? tmp-dir "feature")) "absorb should delete absorbed branch")
    (cleanup tmp-dir)))

(deftest regit-merge-key-flow-dissolve-test
  (let [tmp-dir (init-merge-test-repo "regit-merge-key-dissolve")]
    (git! tmp-dir "checkout" "feature")
    (open-merge-command-from-status! tmp-dir)
    (send-keys "d")
    (choose-first-target!)
    (assert-focused-status tmp-dir "dissolve complete")
    (is= "main" (current-branch tmp-dir))
    (test/assert (path-exists? (path-join tmp-dir "feature.txt")) "dissolve should merge feature file")
    (test/assert (not (branch-present? tmp-dir "feature")) "dissolve should delete source branch")
    (cleanup tmp-dir)))

(deftest regit-merge-key-flow-args-and-strategy-test
  (let [tmp-dir (init-merge-test-repo "regit-merge-key-args-strategy")]
    (open-merge-command-from-status! tmp-dir)
    (send-keys "- f")
    (test/assert (get-in (command-state) [:args :ff-only :value]) "ff-only arg should be enabled")
    (send-keys "- n")
    (test/assert (get-in (command-state) [:args :no-ff :value]) "no-ff arg should be enabled")
    (test/assert (not (get-in (command-state) [:args :ff-only :value])) "no-ff should disable ff-only")
    (send-keys "- s")
    (choose-ours-strategy!)
    (assert-focused-command "--strategy=ours" "strategy arg selected")
    (send-keys "m")
    (choose-first-target!)
    (assert-focused-status tmp-dir "ours strategy merge complete")
    (test/assert (str/includes? (head-subject tmp-dir) "Merge") "strategy merge should create merge commit")
    (test/assert (not (path-exists? (path-join tmp-dir "feature.txt"))) "ours strategy should keep current tree")
    (cleanup tmp-dir)))

(deftest regit-merge-key-flow-ff-only-failure-renders-status-test
  (let [tmp-dir (init-merge-test-repo "regit-merge-key-ff-only-failure")]
    (open-merge-command-from-status! tmp-dir)
    (send-keys "- f")
    (send-keys "m")
    (choose-first-target!)
    (test/assert (str/includes? (messages-content) "Merge failed:")
      (str "ff-only failure should call message, got " (messages-content)))
    (assert-focused-status tmp-dir "ff-only failure")
    (let [content (status-content tmp-dir)]
      (test/assert (str/includes? content "Repository:") "status should render repository heading after failed merge")
      (test/assert (str/includes? content "Recent commits") "status should render recent commits after failed merge"))
    (test/assert (not (path-exists? (path-join tmp-dir "feature.txt"))) "failed merge should not change worktree")
    (cleanup tmp-dir)))
