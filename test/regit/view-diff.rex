(ns regit.tests.view-diff
  (:require [regit.view-diff :as view-diff]
            [rex.base.buffer :as buffer]
            [rex.base.keys :as keys]
            [rex.string :as str]
            [rex.test :as test :refer [deftest is= is-error]]))
(defn- git-cmd [root & args]
  (run-shell* "git" (into ["-C" (str root)] args)))

(defn- git! [root & args]
  (let [result (apply git-cmd root args)]
    (test/assert (zero? (:code result))
      (str "git command failed: " args "\n" (:err result)))))

(defn- repo-with-middle-line-change [name]
  (let [root (temp-file-path name)
        file-path (path-join root "test.txt")]
    (run-shell* "rm" ["-rf" root])
    (run-shell* "mkdir" [root])
    (git! root "init")
    (git! root "config" "user.name" "Rex Test")
    (git! root "config" "user.email" "rex@example.com")
    (write-file file-path "line 1\nline 2\nline 3\n")
    (git! root "add" "test.txt")
    (git! root "commit" "-m" "initial")
    (write-file file-path "line 1\nline 2 changed\nline 3\n")
    root))

(defn- buffer-content [buf]
  (with-read-lock [lock (buffer-text buf)]
    (buffer/slice lock 0 (buffer/len-chars lock))))

(defn- find-line [text needle]
  (let [lines (str/split-lines text)]
    (some (fn [i]
            (when (str/includes? (str (nth lines i)) needle)
              i))
      (range (count lines)))))

(defn- move-to-line [buffer window line]
  (let [pos (with-read-lock [lock (buffer-text buffer)]
              (buffer/line-to-char lock line))]
    (move-cursor pos false window)))

(defn- window-for-buffer [buf]
  (first (filter #(= (:id (window-buffer %)) (:id buf))
           (frame-normal-windows))))

(defn- focused-buffer []
  (window-buffer (focused-window)))

(defn- buffer-line-text [buf line]
  (with-read-lock [lock (buffer-text buf)]
    (str/trim-newline (buffer/text-line lock line))))

(defn- assert-focused-buffer-line [expected-line expected-text]
  (let [target-buffer (focused-buffer)]
    (is= expected-line (current-line target-buffer))
    (is= expected-text (buffer-line-text target-buffer expected-line))
    target-buffer))

(defn- assert-focused-file-line [file-path expected-line expected-text]
  (let [target-buffer (focused-buffer)
        actual-file (:file target-buffer)]
    (test/assert actual-file
      (str "expected focused buffer to have a file path, got " (:name target-buffer)))
    (is= (path-canonicalize file-path) (path-canonicalize actual-file))
    (is= expected-line (current-line target-buffer))
    (is= expected-text (buffer-line-text target-buffer expected-line))
    target-buffer))

(deftest regit-view-diff-keymap-binds-enter-and-control-enter-test
  (is= #'view-diff/regit-view-diff-enter
    (keys/lookup-keymap view-diff/regit-view-diff-keymap
      (keys/parse-key-sequence "<enter>")))
  (is= #'view-diff/regit-view-diff-jump-to-file
    (keys/lookup-keymap view-diff/regit-view-diff-keymap
      (keys/parse-key-sequence "C-<enter>"))))

(deftest regit-view-diff-test
  (let [tmp-dir (temp-file-path "regit-view-diff-test")
        _ (run-shell* "rm" ["-rf" tmp-dir])
        _ (run-shell* "mkdir" [tmp-dir])
        _ (run-shell* "git" ["-C" tmp-dir "init"])
        _ (run-shell* "git" ["-C" tmp-dir "config" "user.name" "Rex Test"])
        _ (run-shell* "git" ["-C" tmp-dir "config" "user.email" "rex@example.com"])
        file-path (path-join tmp-dir "test.txt")
        _ (write-file file-path "line 1\nline 2\n")
        _ (run-shell* "git" ["-C" tmp-dir "add" "test.txt"])
        _ (run-shell* "git" ["-C" tmp-dir "commit" "-m" "initial"])
        _ (write-file file-path "line 1 changed\nline 2\n")
        _ (run-shell* "git" ["-C" tmp-dir "add" "test.txt"])]
    (delete-other-windows)
    (let [buf (create-buffer)]
      (swap! (buffer-state buf) assoc :regit-root tmp-dir)
      (binding [*buffer* buf]
        (view-diff/regit-view-diff tmp-dir)
        (let [view-window (focused-window)
              view-buffer (window-buffer view-window)]
          (binding [*window* view-window
                    *buffer* view-buffer]
            (is= :regit-view-diff *mode*)
            (let [text (with-read-lock [lock (buffer-text)]
                         (buffer/slice lock 0 (buffer/len-chars lock)))]
              (test/assert (str/includes? text "Pending commit")
                (str "regit-view-diff buffer missing heading. Got: " text))
              (test/assert (str/includes? text "Summary:")
                (str "regit-view-diff buffer missing summary. Got: " text))
              (test/assert (str/includes? text "test.txt | 2 +-")
                (str "regit-view-diff buffer missing per-file stat summary. Got: " text))
              (test/assert (str/includes? text "1 file changed, 1 insertion(+), 1 deletion(-)")
                (str "regit-view-diff buffer missing total stat summary. Got: " text))
              (test/assert (str/includes? text "Staged changes (1)")
                (str "regit-view-diff buffer missing staged header. Got: " text))
              (test/assert (str/includes? text "+line 1 changed")
                (str "regit-view-diff buffer missing staged diff. Got: " text)))))))
    (run-shell* "rm" ["-rf" tmp-dir])))

(deftest regit-view-diff-unstaged-summary-test
  (let [tmp-dir (temp-file-path "regit-view-diff-unstaged-summary-test")
        _ (run-shell* "rm" ["-rf" tmp-dir])
        _ (run-shell* "mkdir" [tmp-dir])
        _ (run-shell* "git" ["-C" tmp-dir "init"])
        _ (run-shell* "git" ["-C" tmp-dir "config" "user.name" "Rex Test"])
        _ (run-shell* "git" ["-C" tmp-dir "config" "user.email" "rex@example.com"])
        file-path (path-join tmp-dir "worktree.txt")
        _ (write-file file-path "keep\nremove\n")
        _ (run-shell* "git" ["-C" tmp-dir "add" "worktree.txt"])
        _ (run-shell* "git" ["-C" tmp-dir "commit" "-m" "initial"])
        _ (write-file file-path "keep\nadded\n")]
    (delete-other-windows)
    (let [buf (create-buffer)]
      (swap! (buffer-state buf) assoc :regit-root tmp-dir)
      (binding [*buffer* buf]
        (view-diff/regit-view-diff tmp-dir :unstaged)
        (let [view-window (focused-window)
              view-buffer (window-buffer view-window)]
          (binding [*window* view-window
                    *buffer* view-buffer]
            (is= :regit-view-diff *mode*)
            (let [text (with-read-lock [lock (buffer-text)]
                         (buffer/slice lock 0 (buffer/len-chars lock)))]
              (test/assert (str/includes? text "Working tree diff")
                (str "regit-view-diff buffer missing unstaged heading. Got: " text))
              (test/assert (str/includes? text "Summary:")
                (str "regit-view-diff buffer missing summary. Got: " text))
              (test/assert (str/includes? text "worktree.txt | 2 +-")
                (str "regit-view-diff buffer missing per-file summary. Got: " text))
              (test/assert (str/includes? text "1 file changed, 1 insertion(+), 1 deletion(-)")
                (str "regit-view-diff buffer missing total summary. Got: " text))
              (test/assert (str/includes? text "Unstaged changes (1)")
                (str "regit-view-diff buffer missing unstaged header. Got: " text))
              (test/assert (str/includes? text "+added")
                (str "regit-view-diff buffer missing unstaged diff. Got: " text)))))))
    (run-shell* "rm" ["-rf" tmp-dir])))

(deftest regit-view-diff-enter-on-added-hunk-line-jumps-to-added-line-test
  (let [root (repo-with-middle-line-change "regit-view-diff-enter-added-line")
        file-path (path-join root "test.txt")]
    (try
      (delete-other-windows)
      (let [view (view-diff/regit-view-diff root :unstaged)
            view-window (or (window-for-buffer view) (focused-window))
            line (find-line (buffer-content view) "+line 2 changed")]
        (test/assert line "expected added hunk line in diff view")
        (set-focused-window view-window)
        (binding [*buffer* view
                  *window* view-window]
          (move-to-line view view-window line)
          (view-diff/regit-view-diff-enter))
        (assert-focused-file-line file-path 1 "line 2 changed"))
      (finally
        (run-shell* "rm" ["-rf" root])))))

(deftest regit-view-diff-enter-on-removed-staged-hunk-line-opens-base-line-test
  (let [root (repo-with-middle-line-change "regit-view-diff-enter-removed-staged")]
    (try
      (delete-other-windows)
      (git! root "add" "test.txt")
      (let [view (view-diff/regit-view-diff root :staged)
            view-window (or (window-for-buffer view) (focused-window))
            line (find-line (buffer-content view) "-line 2")]
        (test/assert line "expected removed hunk line in staged diff view")
        (set-focused-window view-window)
        (binding [*buffer* view
                  *window* view-window]
          (move-to-line view view-window line)
          (view-diff/regit-view-diff-enter))
        (let [target-buffer (assert-focused-buffer-line 1 "line 2")]
          (test/assert (str/includes? (:name target-buffer) "*staged-base: test.txt*")
            (str "expected staged base synthetic buffer, got " (:name target-buffer)))))
      (finally
        (run-shell* "rm" ["-rf" root])))))

(deftest regit-view-diff-opened-from-status-enter-on-added-staged-line-opens-index-line-test
  (let [root (repo-with-middle-line-change "regit-view-diff-status-staged-added-line")]
    (try
      (delete-other-windows)
      (git! root "add" "test.txt")
      (let [status-buffer (create-buffer)]
        (binding [*buffer* status-buffer]
          (call-var regit.status/regit-status root)
          (let [status-window (focused-window)
                status-buffer (window-buffer status-window)
                staged-line (find-line (buffer-content status-buffer) "Staged changes")]
            (test/assert staged-line "expected staged changes section in status buffer")
            (binding [*buffer* status-buffer
                      *window* status-window]
              (move-to-line status-buffer status-window staged-line)
              (call-var regit.status/regit-status-enter))
            (let [view-window (focused-window)
                  view-buffer (window-buffer view-window)
                  added-line (find-line (buffer-content view-buffer) "+line 2 changed")]
              (test/assert added-line "expected added hunk line in staged diff view")
              (binding [*buffer* view-buffer
                        *window* view-window]
                (move-to-line view-buffer view-window added-line)
                (view-diff/regit-view-diff-enter))
              (let [target-buffer (assert-focused-buffer-line 1 "line 2 changed")]
                (test/assert (str/includes? (:name target-buffer) "*staged: test.txt*")
                  (str "expected staged synthetic buffer, got " (:name target-buffer))))))))
      (finally
        (run-shell* "rm" ["-rf" root])))))

(deftest regit-view-diff-jump-to-file-on-removed-staged-hunk-line-jumps-to-working-line-test
  (let [root (repo-with-middle-line-change "regit-view-diff-jump-file-removed-staged")
        file-path (path-join root "test.txt")]
    (try
      (delete-other-windows)
      (git! root "add" "test.txt")
      (let [view (view-diff/regit-view-diff root :staged)
            view-window (or (window-for-buffer view) (focused-window))
            line (find-line (buffer-content view) "-line 2")]
        (test/assert line "expected removed hunk line in staged diff view")
        (set-focused-window view-window)
        (binding [*buffer* view
                  *window* view-window]
          (move-to-line view view-window line)
          (view-diff/regit-view-diff-jump-to-file))
        (assert-focused-file-line file-path 1 "line 2 changed"))
      (finally
        (run-shell* "rm" ["-rf" root])))))
