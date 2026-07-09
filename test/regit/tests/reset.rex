(ns regit.tests.reset
  (:require [regit.reset :as reset]
            [regit.status :as status]
            [regit.log :as log]
            [regit.command :as regit-command]
            [rex.ui.iselect :as iselect]
            [rex.base.buffer :as buffer]
            [rex.base.keys :as keys]
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

(defn- commit-file! [root file content subject]
  (write-file (path-join root file) content)
  (git! root "add" file)
  (git! root "commit" "-m" subject))

(defn- init-reset-test-repo [name]
  (let [tmp-dir (temp-file-path name)
        _ (run-shell* "rm" ["-rf" tmp-dir])
        _ (run-shell* "mkdir" [tmp-dir])
        _ (git! tmp-dir "init")
        _ (git! tmp-dir "config" "user.email" "test@example.com")
        _ (git! tmp-dir "config" "user.name" "Test User")
        _ (commit-file! tmp-dir "base.txt" "base\n" "base")
        _ (git! tmp-dir "branch" "-M" "main")
        _ (commit-file! tmp-dir "one.txt" "one\n" "one")
        _ (git! tmp-dir "branch" "backup" "HEAD")
        _ (commit-file! tmp-dir "two.txt" "two\n" "two")
        _ (git! tmp-dir "update-ref" "refs/remotes/origin/main" "HEAD")
        _ (git! tmp-dir "tag" "v-test" "HEAD~1")]
    tmp-dir))

(defn- cleanup [root]
  (run-shell* "rm" ["-rf" root]))

(defn- short-hash [root rev]
  (git-out root "rev-parse" "--short" rev))

(defn- current-head [root]
  (short-hash root "HEAD"))

(defn- focused-buffer []
  (window-buffer (focused-window)))

(defn- buffer-content [buf]
  (with-read-lock [lock (buffer-text buf)]
    (buffer/slice lock 0 (buffer/len-chars lock))))

(defn- focused-display-content []
  (str/strip-properties (buffer-content (focused-buffer))))

(defn- find-line [content needle]
  (first (remove nil?
           (map-indexed (fn [idx line]
                          (when (str/includes? line needle)
                            idx))
             (str/split-lines content)))))

(defn- move-focused-line! [line]
  (let [win (focused-window)
        buf (window-buffer win)]
    (move-cursor (with-read-lock [lock (buffer-text buf)]
                   (buffer/line-to-char lock line))
      false win)))

(defn- move-focused-line-containing! [needle]
  (let [line (find-line (focused-display-content) needle)]
    (test/assert line (str "Could not find line containing " needle " in:\n" (focused-display-content)))
    (move-focused-line! line)
    line))

(defn- command-for [key]
  (let [ui-win (minibuffer-ui-window)]
    (test/assert ui-win "regit command UI not open")
    (let [ui-buf (window-buffer ui-win)
          cmd (binding [*buffer* ui-buf]
                (regit-command/regit-command-keymap (keys/parse-key-sequence key)))]
      (when cmd
        (fn []
          (binding [*buffer* ui-buf]
            (cmd)))))))

(defn- invoke-status-key! [key]
  (let [win (focused-window)
        buf (window-buffer win)
        cmd (keys/lookup-keymap status/regit-status-keymap (keys/parse-key-sequence key))]
    (test/assert (ifn? cmd) (str "missing regit-status key " key))
    (binding [*buffer* buf]
      (cmd))))

(defn- invoke-log-key! [key]
  (let [win (focused-window)
        buf (window-buffer win)
        cmd (keys/lookup-keymap log/regit-log-keymap (keys/parse-key-sequence key))]
    (test/assert (ifn? cmd) (str "missing regit-log key " key))
    (binding [*buffer* buf]
      (cmd))))

(defn- invoke-reset-action! [key]
  (let [cmd (command-for key)]
    (test/assert (ifn? cmd) (str "missing regit reset action " key))
    (cmd)))

(defn- select-current-iselect-entry! []
  (let [mb-win (minibuffer-window)]
    (test/assert mb-win "iselect minibuffer not opened")
    (binding [*buffer* (window-buffer mb-win)]
      (iselect/select-current-entry))))

(defn- assert-reset-iselect-default [expected-hash]
  (let [mb-win (minibuffer-window)
        mb-buf (when mb-win (window-buffer mb-win))]
    (test/assert mb-buf "iselect minibuffer not opened")
    (binding [*buffer* mb-buf]
      (test/assert (= :iselect *mode*) (str "expected iselect mode, got " *mode*))
      (is= expected-hash (iselect/current-input))
      (let [entries (:entries @iselect/*state*)]
        (test/assert (some #(= % expected-hash) entries)
          (str "context commit " expected-hash " missing from reset targets: " entries))))))

(deftest regit-reset-command-layout-and-bindings-test
  (let [tmp-dir (init-reset-test-repo "regit-reset-command-layout")]
    (reset/regit-reset tmp-dir)
    (let [ui-win (minibuffer-ui-window)]
      (test/assert ui-win "regit-reset did not open command UI")
      (let [ui-buf (window-buffer ui-win)]
        (binding [*buffer* ui-buf]
          (let [st @regit-command/*state*
                text (buffer-content ui-buf)]
            (doseq [key ["b" "f" "m" "s" "h" "k" "i" "w"]]
              (test/assert (contains? (:actions st) key) (str "missing reset action " key)))
            (test/assert (str/includes? text "Reset this") (str "reset UI missing Reset this heading. Got:\n" text))
            (test/assert (str/includes? text "hard     (HEAD, index and worktree)")
              (str "reset UI missing hard action. Got:\n" text))
            (test/assert (ifn? (keys/lookup-keymap status/regit-status-keymap (keys/parse-key-sequence "O")))
              "regit-status did not bind O to reset")
            (test/assert (ifn? (keys/lookup-keymap log/regit-log-keymap (keys/parse-key-sequence "O")))
              "regit-log did not bind O to reset"))
          (when-let [close-cmd (command-for "q")]
            (close-cmd)))))
    (cleanup tmp-dir)))

(deftest regit-reset-status-context-hard-resets-to-commit-at-point-test
  (let [tmp-dir (init-reset-test-repo "regit-reset-status-context")
        target (short-hash tmp-dir "HEAD~1")]
    (status/regit-status tmp-dir)
    (move-focused-line-containing! "one")
    (invoke-status-key! "O")
    (invoke-reset-action! "h")
    (assert-reset-iselect-default target)
    (select-current-iselect-entry!)
    (is= target (current-head tmp-dir))
    (cleanup tmp-dir)))

(deftest regit-reset-log-context-soft-resets-to-commit-at-point-test
  (let [tmp-dir (init-reset-test-repo "regit-reset-log-context")
        target (short-hash tmp-dir "HEAD~1")]
    (log/regit-log-target tmp-dir "main")
    (move-focused-line-containing! "one")
    (invoke-log-key! "O")
    (invoke-reset-action! "s")
    (assert-reset-iselect-default target)
    (select-current-iselect-entry!)
    (is= target (current-head tmp-dir))
    (cleanup tmp-dir)))
