(ns regit.tests.commit
  (:require [regit.commit :as commit]
            [regit.status :as status]
            [rex.base.buffer :as buffer]
            [rex.base.frame :as frame]
            [rex.base.keys :as keys]
            [rex.string :as str]
            [rex.test :as test :refer [deftest is= is-error]]))

(defn- git [root & args]
  (run-shell* "git" (into ["-C" root] args)))

(defn- git! [root & args]
  (let [result (apply git root args)]
    (test/assert (zero? (:code result))
      (str "git command failed: " args "\n" (:err result) (:out result)))
    result))

(defn- git-out [root & args]
  (str/trim (:out (apply git! root args))))

(defn- init-commit-test-repo [name]
  (let [tmp-dir (temp-file-path name)
        _ (run-shell* "rm" ["-rf" tmp-dir])
        _ (run-shell* "mkdir" [tmp-dir])
        _ (git! tmp-dir "init")
        _ (git! tmp-dir "config" "user.name" "Rex Test")
        _ (git! tmp-dir "config" "user.email" "rex@example.com")
        file-path (path-join tmp-dir "test.txt")
        _ (write-file file-path "initial\n")
        _ (git! tmp-dir "add" "test.txt")
        _ (git! tmp-dir "commit" "-m" "initial")]
    tmp-dir))

(defn- stage-file! [root file content]
  (write-file (path-join root file) content)
  (git! root "add" file))

(defn- cleanup [root]
  (run-shell* "rm" ["-rf" root]))

(defn- head-subject [root]
  (git-out root "log" "-1" "--pretty=%s"))

(defn- buffer-content [buf]
  (with-read-lock [lock (buffer-text buf)]
    (buffer/slice lock 0 (buffer/len-chars lock))))

(defn- focused-buffer []
  (window-buffer (focused-window)))

(defn- focused-buffer-name []
  (:name (focused-buffer)))

(defn- focused-content []
  (buffer-content (focused-buffer)))

(defn- minibuffer-ui-content []
  (buffer-content (window-buffer (minibuffer-ui-window))))

(defn- messages-content []
  (buffer-content (buffer/get-buffer "*Messages*")))

(defn- send-keys [keys-str]
  (let [frame-id (:id *frame*)]
    (frame/set-pending-sequence! frame-id [])
    (frame/set-numeric-prefix! frame-id nil))
  (doseq [keyspec (keys/parse-key-sequence keys-str)]
    (let [win (focused-window)]
      (binding [*buffer* (window-buffer win)
                *frame* (window-frame win)]
        (frame/process-key-event keyspec)))))

(defn- wait-for-message [needle context]
  (test/wait-for [content (messages-content)]
    :until (str/includes? content needle)
    :timeout-message (fn [] (str context ": expected message " needle ", got " (messages-content)))))

(defn- assert-focused-status [root context]
  (test/assert (= (status/find-status-buffer root) (focused-buffer))
    (str context ": expected focused regit-status buffer, got " (focused-buffer-name)))
  (test/assert (str/includes? (focused-content) "Repository:")
    (str context ": missing status repository heading")))

(defn- assert-focused-command [needle context]
  (test/assert (= (minibuffer-ui-window) (focused-window))
    (str context ": expected minibuffer UI focus, got " (focused-buffer-name)))
  (test/assert (str/includes? (minibuffer-ui-content) needle)
    (str context ": missing command text " needle)))

(defn- open-status-commit-action! [root key context]
  (status/regit-status root)
  (assert-focused-status root (str context " before commit command"))
  (send-keys "c")
  (assert-focused-command "Commit" (str context " commit command"))
  (send-keys key))

(defn- assert-focused-regit-commit-message [context]
  (let [buf (focused-buffer)
        state @(buffer-state buf)]
    (test/assert (str/includes? (:name buf) "regit-commit-message")
      (str context ": expected regit commit message buffer, got " (:name buf)))
    (binding [*buffer* buf]
      (test/assert (= :regit-commit-message *mode*) (str context ": expected :regit-commit-message mode, got " *mode*))
      (test/assert (contains? (set *submodes*) :vim)
        (str context ": vim submode not active. Current submodes: " *submodes*)))
    (test/assert (:regit-commit-editor-state state)
      (str context ": commit message buffer is not backed by intercepted git editor state"))
    (test/assert (:regit-commit-message-path state)
      (str context ": missing git commit message file path"))
    buf))

(defn- assert-focused-commit-comments-dimmed [context]
  (let [content (focused-content)
        comment-pos (str/index-of content "#")]
    (test/assert comment-pos
      (str context ": expected git comments in commit message buffer. Got:\n" content))
    (binding [*buffer* (focused-buffer)]
      (test/assert (not (empty? (buffer/property-at comment-pos)))
        (str context ": expected comment text to have face properties")))))

(defn- find-staged-diff-window [exclude-window]
  (some (fn [w]
          (let [buf (window-buffer w)]
            (when (and (not= w exclude-window)
                    (str/includes? (:name buf) "*regit-diff: "))
              w)))
    (frame-normal-windows)))

(deftest regit-commit-intercepts-git-editor-from-status-test
  (let [tmp-dir (init-commit-test-repo "regit-commit-intercepts-git-editor")
        file-path (path-join tmp-dir "test.txt")]
    (try
      (stage-file! tmp-dir "test.txt" "changed\n")
      (delete-other-windows)
      (open-file file-path)
      (open-status-commit-action! tmp-dir "c" "regular commit")
      (assert-focused-regit-commit-message "regular commit")
      (test/assert (str/includes? (focused-buffer-name) "regit-commit-message")
        (str "regular commit should use regular commit message buffer label, got " (focused-buffer-name)))
      (test/assert (str/includes? (focused-content) "# Please enter the commit message")
        (str "regular commit editor should contain git commit comments. Got:\n" (focused-content)))
      (assert-focused-commit-comments-dimmed "regular commit")
      (is= 0 (cursor-position))
      (let [finish-cmd (commit/commit-message-keymap (keys/parse-key-sequence "C-c C-c"))]
        (test/assert (ifn? finish-cmd) "could not find C-c C-c binding"))
      (set-string "intercepted regular commit\n" (focused-buffer))
      (send-keys "C-c C-c")
      (test/wait-for [subject (head-subject tmp-dir)]
        :until (= "intercepted regular commit" subject)
        :timeout-message "regular commit editor submit did not create the edited commit")
      (wait-for-message "Committed" "regular commit submit")
      (finally
        (cleanup tmp-dir)))))

(deftest regit-amend-intercepts-git-editor-from-status-test
  (let [tmp-dir (init-commit-test-repo "regit-amend-intercepts-git-editor")
        file-path (path-join tmp-dir "test.txt")]
    (try
      (delete-other-windows)
      (open-file file-path)
      (open-status-commit-action! tmp-dir "a" "amend")
      (assert-focused-regit-commit-message "amend")
      (test/assert (str/includes? (focused-content) "initial")
        (str "amend editor should contain the existing commit message. Got:\n" (focused-content)))
      (assert-focused-commit-comments-dimmed "amend")
      (set-string "intercepted amend commit\n" (focused-buffer))
      (send-keys "C-c C-c")
      (test/wait-for [subject (head-subject tmp-dir)]
        :until (= "intercepted amend commit" subject)
        :timeout-message "amend editor submit did not update the head commit message")
      (wait-for-message "Amended" "amend submit")
      (finally
        (cleanup tmp-dir)))))

(deftest regit-commit-opens-staged-diff-test
  (let [tmp-dir (init-commit-test-repo "regit-commit-opens-staged-diff")
        file-path (path-join tmp-dir "test.txt")]
    (try
      (stage-file! tmp-dir "test.txt" "line 1 changed\nline 2\n")
      (delete-other-windows)
      (open-file file-path)
      (open-status-commit-action! tmp-dir "c" "staged diff")
      (let [msg-window (focused-window)
            msg-buffer (focused-buffer)]
        (assert-focused-regit-commit-message "staged diff")
        (let [diff-window (find-staged-diff-window msg-window)]
          (test/assert diff-window "regit-view-diff window not opened")
          (let [diff-buffer (window-buffer diff-window)]
            (is= diff-buffer (:regit-view-diff-buffer @(buffer-state msg-buffer)))
            (binding [*window* diff-window
                      *buffer* diff-buffer]
              (is= :regit-view-diff *mode*)
              (let [text (buffer-content diff-buffer)]
                (test/assert (str/includes? text "Staged changes (1)")
                  (str "regit-view-diff buffer missing staged header. Got: " text))
                (test/assert (str/includes? text "+line 1 changed")
                  (str "regit-view-diff buffer missing staged diff. Got: " text)))))))
      (send-keys "C-c C-k")
      (wait-for-message "Aborted commit message edit" "staged diff abort")
      (finally
        (cleanup tmp-dir)))))

(deftest regit-commit-kills-staged-diff-after-submit-test
  (let [tmp-dir (init-commit-test-repo "regit-commit-kills-staged-diff-after-submit")
        file-path (path-join tmp-dir "test.txt")]
    (try
      (stage-file! tmp-dir "test.txt" "line 1\nline 2\n")
      (delete-other-windows)
      (open-file file-path)
      (open-status-commit-action! tmp-dir "c" "kill staged diff")
      (let [msg-buffer (focused-buffer)
            diff-buffer (:regit-view-diff-buffer @(buffer-state msg-buffer))
            diff-name (:name diff-buffer)]
        (test/assert diff-buffer "commit message buffer missing associated regit-view-diff buffer")
        (set-string "commit that closes staged diff\n" msg-buffer)
        (send-keys "C-c C-c")
        (test/assert (not (some #(= (:name %) diff-name) (list-buffers)))
          "associated regit-view-diff buffer was not killed after submitting git commit message")
        (test/wait-for [subject (head-subject tmp-dir)]
          :until (= "commit that closes staged diff" subject)
          :timeout-message "regular commit editor submit did not create commit"))
      (finally
        (cleanup tmp-dir)))))
