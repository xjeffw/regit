(ns regit.tests.log-select
  (:require [regit.log-select :as log-select]
            [regit.tests.util :refer [buffer-content cleanup git! git-out]]
            [rex.base.buffer :as buffer]
            [rex.string :as str]
            [rex.test :as test :refer [deftest is=]]))

(defn- init-log-select-repo [name]
  (let [tmp-dir (temp-file-path name)
        _ (run-shell* "rm" ["-rf" tmp-dir] {:direnv false})
        _ (run-shell* "mkdir" [tmp-dir] {:direnv false})
        _ (git! tmp-dir "init")
        _ (git! tmp-dir "config" "user.email" "test@example.com")
        _ (git! tmp-dir "config" "user.name" "Test User")
        _ (write-file (path-join tmp-dir "one.txt") "one\n")
        _ (git! tmp-dir "add" "one.txt")
        _ (git! tmp-dir "commit" "-m" "one")
        _ (git! tmp-dir "branch" "-M" "main")
        _ (write-file (path-join tmp-dir "two.txt") "two\n")
        _ (git! tmp-dir "add" "two.txt")
        _ (git! tmp-dir "commit" "-m" "two")
        _ (write-file (path-join tmp-dir "three.txt") "three\n")
        _ (git! tmp-dir "add" "three.txt")
        _ (git! tmp-dir "commit" "-m" "three")]
    tmp-dir))

(deftest regit-log-select-opens-and-picks-commit-test
  (let [tmp-dir (init-log-select-repo "regit-log-select-pick")
        selected (atom nil)
        expected (git-out tmp-dir "log" "-1" "--format=%h")
        buffer (log-select/regit-log-select tmp-dir
                 "Type C-c C-c on a commit to select it, or C-c C-k to abort"
                 (fn [commit] (reset! selected commit)))]
    (let [window (focused-window)
          content (buffer-content buffer)]
      (test/assert (str/includes? content "Type C-c C-c on a commit") "missing selection message")
      (test/assert (str/includes? content "three") "missing newest commit")
      (binding [*buffer* buffer
                *window* window
                *mode* :regit-log-select
                *submodes* #{}]
        (move-cursor (with-read-lock [lock (buffer-text buffer)]
                       (buffer/line-to-char lock 1))
          false
          window)
        (log-select/regit-log-select-pick))
      (is= expected @selected))
    (cleanup tmp-dir)))

(deftest regit-log-select-quit-calls-quit-function-test
  (let [tmp-dir (init-log-select-repo "regit-log-select-quit")
        quit? (atom false)
        buffer (log-select/regit-log-select tmp-dir
                 "Select a commit"
                 (fn [_commit] nil)
                 {:quit-fn (fn [] (reset! quit? true))})]
    (binding [*buffer* buffer
              *window* (focused-window)
              *mode* :regit-log-select
              *submodes* #{}]
      (log-select/regit-log-select-quit))
    (is= true @quit?)
    (cleanup tmp-dir)))
