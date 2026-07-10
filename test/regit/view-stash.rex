(ns regit.tests.view-stash
  (:require [regit.tests.util :refer [assert-focused-buffer-line
                                      assert-focused-file-line
                                      buffer-content
                                      find-line
                                      git!
                                      git-cmd
                                      move-to-line
                                      repo-with-middle-line-change]]
            [regit.view-stash :as view-stash]
            [rex.base.buffer :as buffer]
            [rex.base.keys :as keys]
            [rex.string :as str]
            [rex.test :as test :refer [deftest is= is-error]]))

(deftest regit-view-stash-keymap-binds-enter-and-control-enter-test
  (is= #'view-stash/regit-view-stash-enter
    (keys/lookup-keymap view-stash/regit-view-stash-keymap
      (keys/parse-key-sequence "<enter>")))
  (is= #'view-stash/regit-view-stash-jump-to-file
    (keys/lookup-keymap view-stash/regit-view-stash-keymap
      (keys/parse-key-sequence "C-<enter>"))))

(deftest regit-view-stash-shows-separate-summaries-test
  (let [tmp-dir (temp-file-path "regit-view-stash-shows-separate-summaries-test")
        _ (run-shell* "rm" ["-rf" tmp-dir] {:direnv false})
        _ (run-shell* "mkdir" [tmp-dir] {:direnv false})
        _ (run-shell* "git" ["-C" tmp-dir "init"] {:direnv false})
        _ (run-shell* "git" ["-C" tmp-dir "config" "user.name" "Rex Test"] {:direnv false})
        _ (run-shell* "git" ["-C" tmp-dir "config" "user.email" "rex@example.com"] {:direnv false})
        staged-path (path-join tmp-dir "index.txt")
        unstaged-path (path-join tmp-dir "worktree.txt")
        _ (write-file staged-path "keep\nremove\n")
        _ (write-file unstaged-path "keep\nremove\n")
        _ (run-shell* "git" ["-C" tmp-dir "add" "index.txt" "worktree.txt"] {:direnv false})
        _ (run-shell* "git" ["-C" tmp-dir "commit" "-m" "initial"] {:direnv false})
        _ (write-file staged-path "keep\nstaged\n")
        _ (run-shell* "git" ["-C" tmp-dir "add" "index.txt"] {:direnv false})
        _ (write-file unstaged-path "keep\nunstaged\n")
        _ (run-shell* "git" ["-C" tmp-dir "stash" "push" "-m" "summary stash"] {:direnv false})]
    (delete-other-windows)
    (let [buf (create-buffer)]
      (swap! (buffer-state buf) assoc :regit-root tmp-dir)
      (binding [*buffer* buf]
        (view-stash/regit-view-stash "stash@{0}")
        (let [view-window (focused-window)
              view-buffer (window-buffer view-window)]
          (binding [*window* view-window
                    *buffer* view-buffer]
            (is= :regit-view-stash *mode*)
            (let [text (with-read-lock [lock (buffer-text)]
                         (buffer/slice lock 0 (buffer/len-chars lock)))
                  label (:label @(buffer-state view-buffer))]
              (test/assert (property-string? label) "regit-view-stash label should preserve styled stash name")
              (test/assert (str/includes? label "regit-stash: ") "regit-view-stash label missing prefix")
              (test/assert (str/includes? label "regit-view-stash-shows-separate-summaries-test") "regit-view-stash label missing repo name")
              (test/assert (str/includes? label "stash@{0}") "regit-view-stash label missing stash name")
              (test/assert (str/includes? text "Staged summary:")
                (str "regit-view-stash buffer missing staged summary. Got: " text))
              (test/assert (str/includes? text "index.txt | 2 +-")
                (str "regit-view-stash buffer missing staged per-file summary. Got: " text))
              (test/assert (str/includes? text "Unstaged summary:")
                (str "regit-view-stash buffer missing unstaged summary. Got: " text))
              (test/assert (str/includes? text "worktree.txt | 2 +-")
                (str "regit-view-stash buffer missing unstaged per-file summary. Got: " text)))))))
    (run-shell* "rm" ["-rf" tmp-dir] {:direnv false})))

(deftest regit-view-stash-apply-test
  (let [tmp-dir (temp-file-path "regit-view-stash-apply-test")
        _ (run-shell* "rm" ["-rf" tmp-dir] {:direnv false})
        _ (run-shell* "mkdir" [tmp-dir] {:direnv false})
        _ (run-shell* "git" ["-C" tmp-dir "init"] {:direnv false})
        _ (run-shell* "git" ["-C" tmp-dir "config" "user.name" "Rex Test"] {:direnv false})
        _ (run-shell* "git" ["-C" tmp-dir "config" "user.email" "rex@example.com"] {:direnv false})
        file-path (path-join tmp-dir "test.txt")
        _ (write-file file-path "line 1\nline 2\n")
        _ (run-shell* "git" ["-C" tmp-dir "add" "test.txt"] {:direnv false})
        _ (run-shell* "git" ["-C" tmp-dir "commit" "-m" "initial"] {:direnv false})
        _ (write-file file-path "line 1 modified\nline 2\n")
        _ (run-shell* "git" ["-C" tmp-dir "stash" "push" "-m" "test stash"] {:direnv false})
        _ (write-file file-path "line 1\nline 2\n")]
    (let [status-buffer (create-buffer)]
      (binding [*buffer* status-buffer]
        (call-var regit.status/regit-status tmp-dir)
        (let [status-window (focused-window)
              status-buffer (window-buffer status-window)]
          (binding [*window* status-window
                    *buffer* status-buffer]
            (let [lines (with-read-lock [lock (buffer-text)] (str/split-lines (buffer/slice lock 0 (buffer/len-chars lock))))
                  stash-line-idx (some (fn [i] (when (str/includes? (nth lines i) "test stash") i))
                                   (range (count lines)))]
              (test/assert stash-line-idx "Could not find test stash")
              (move-cursor (with-read-lock [lock (buffer-text status-buffer)]
                             (buffer/line-to-char lock stash-line-idx))
                false status-window)
              (call-var regit.status/regit-status-enter)
              (let [view-window (focused-window)
                    view-buffer (window-buffer view-window)]
                (binding [*window* view-window
                          *buffer* view-buffer]
                  (let [view-lines (with-read-lock [lock (buffer-text)] (str/split-lines (buffer/slice lock 0 (buffer/len-chars lock))))
                        header-line-idx (some (fn [i] (when (str/includes? (nth view-lines i) "modified test.txt") i))
                                          (range (count view-lines)))]
                    (test/assert header-line-idx "Could not find file header")
                    (let [expanded-lines (with-read-lock [lock (buffer-text)] (str/split-lines (buffer/slice lock 0 (buffer/len-chars lock))))
                          hunk-line-idx (some (fn [i] (when (str/includes? (nth expanded-lines i) "+line 1 modified") i))
                                          (range (count expanded-lines)))]
                      (test/assert hunk-line-idx "Could not find hunk after expansion")
                      (move-cursor (with-read-lock [lock (buffer-text view-buffer)]
                                     (buffer/line-to-char lock hunk-line-idx))
                        false view-window)
                      (view-stash/regit-view-stash-apply)
                      (set-focused-window status-window)
                      (let [status-text (with-read-lock [lock (buffer-text status-buffer)]
                                          (buffer/slice lock 0 (buffer/len-chars lock)))]
                        (test/assert (str/includes? status-text "test.txt")
                          (str "Status buffer missing test.txt. Got: " status-text))
                        (test/assert (str/includes? status-text "Unstaged changes")
                          (str "Status buffer missing Unstaged changes. Got: " status-text)))))))))))
      (run-shell* "rm" ["-rf" tmp-dir] {:direnv false}))))

(deftest regit-view-stash-enter-on-added-hunk-line-jumps-to-stash-line-test
  (let [root (repo-with-middle-line-change "regit-view-stash-enter-added-line")]
    (try
      (delete-other-windows)
      (git! root "stash" "push" "-m" "sample stash")
      (let [source-buffer (create-buffer true)]
        (swap! (buffer-state source-buffer) assoc :regit-root root)
        (binding [*buffer* source-buffer]
          (let [view (view-stash/regit-view-stash "stash@{0}")
                view-window (focused-window)
                line (find-line (buffer-content view) "+line 2 changed")]
            (test/assert line "expected added hunk line in stash view")
            (move-to-line view view-window line)
            (view-stash/regit-view-stash-enter)
            (let [target-buffer (assert-focused-buffer-line 1 "line 2 changed")]
              (test/assert (str/includes? (:name target-buffer) "*stash[stash@{0}]: test.txt*")
                (str "expected stash synthetic buffer, got " (:name target-buffer)))))))
      (finally
        (run-shell* "rm" ["-rf" root] {:direnv false})))))

(deftest regit-view-stash-enter-on-removed-hunk-line-jumps-to-stash-index-line-test
  (let [root (repo-with-middle-line-change "regit-view-stash-enter-removed-line")]
    (try
      (delete-other-windows)
      (git! root "stash" "push" "-m" "sample stash")
      (let [source-buffer (create-buffer true)]
        (swap! (buffer-state source-buffer) assoc :regit-root root)
        (binding [*buffer* source-buffer]
          (let [view (view-stash/regit-view-stash "stash@{0}")
                view-window (focused-window)
                line (find-line (buffer-content view) "-line 2")]
            (test/assert line "expected removed hunk line in stash view")
            (move-to-line view view-window line)
            (view-stash/regit-view-stash-enter)
            (let [target-buffer (assert-focused-buffer-line 1 "line 2")]
              (test/assert (str/includes? (:name target-buffer) "*stash[stash@{0}^2]: test.txt*")
                (str "expected stash index synthetic buffer, got " (:name target-buffer)))))))
      (finally
        (run-shell* "rm" ["-rf" root] {:direnv false})))))

(deftest regit-view-stash-jump-to-file-on-added-hunk-line-jumps-to-working-line-test
  (let [root (repo-with-middle-line-change "regit-view-stash-jump-file-added-line")
        file-path (path-join root "test.txt")]
    (try
      (delete-other-windows)
      (git! root "stash" "push" "-m" "sample stash")
      (let [source-buffer (create-buffer true)]
        (swap! (buffer-state source-buffer) assoc :regit-root root)
        (binding [*buffer* source-buffer]
          (let [view (view-stash/regit-view-stash "stash@{0}")
                view-window (focused-window)
                line (find-line (buffer-content view) "+line 2 changed")]
            (test/assert line "expected added hunk line in stash view")
            (move-to-line view view-window line)
            (view-stash/regit-view-stash-jump-to-file)
            (assert-focused-file-line file-path 1 "line 2"))))
      (finally
        (run-shell* "rm" ["-rf" root] {:direnv false})))))
