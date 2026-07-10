(ns regit.tests.util
  (:require [regit.command :as regit-command]
            [regit.status :as status]
            [rex.base.buffer :as buffer]
            [rex.base.frame :as frame]
            [rex.base.keys :as keys]
            [rex.base.theme :as theme]
            [rex.string :as str]
            [rex.test :as test :refer [is=]]
            [rex.ui.iselect :as iselect]
            [rex.ui.simple-prompt :as simple-prompt]))

(defn sh! [command args]
  (run-shell* command args {:direnv false}))

(defn git [root & args]
  (sh! "git" (into ["-C" (str root)] args)))

(defn git-cmd [root & args]
  (apply git root args))

(defn git! [root & args]
  (let [result (apply git root args)]
    (test/assert (zero? (:code result))
      (str "git command failed: " args "\n" (:err result) (:out result)))
    result))

(defn git-out [root & args]
  (str/trim (:out (apply git! root args))))

(defn git-output [root & args]
  (apply git-out root args))

(defn git-config [root key]
  (let [{:keys [code out]} (git root "config" "--get" key)]
    (when (zero? code)
      (str/trim out))))

(defn commit-file! [root file content subject]
  (write-file (path-join root file) content)
  (git! root "add" file)
  (git! root "commit" "-m" subject))

(defn init-test-repo [name]
  (let [root (temp-file-path name)]
    (sh! "rm" ["-rf" root])
    (sh! "mkdir" [root])
    (git! root "init")
    (git! root "config" "user.email" "test@example.com")
    (git! root "config" "user.name" "Test User")
    (commit-file! root "test.txt" "hello\n" "initial")
    (git! root "branch" "-M" "main")
    root))

(defn repo-with-middle-line-change [name]
  (let [root (temp-file-path name)
        file-path (path-join root "test.txt")]
    (sh! "rm" ["-rf" root])
    (sh! "mkdir" [root])
    (git! root "init")
    (git! root "config" "user.name" "Rex Test")
    (git! root "config" "user.email" "rex@example.com")
    (write-file file-path "line 1\nline 2\nline 3\n")
    (git! root "add" "test.txt")
    (git! root "commit" "-m" "initial")
    (write-file file-path "line 1\nline 2 changed\nline 3\n")
    root))

(defn cleanup [& paths]
  (doseq [path paths]
    (when path
      (sh! "rm" ["-rf" path]))))

(defn current-branch [root]
  (git-out root "rev-parse" "--abbrev-ref" "HEAD"))

(defn head-subject [root]
  (git-out root "log" "-1" "--pretty=%s"))

(defn branch-present? [root branch]
  (zero? (:code (git root "show-ref" "--verify" "--quiet"
                  (str "refs/heads/" branch)))))

(defn buffer-content [buf]
  (with-read-lock [lock (buffer-text buf)]
    (buffer/slice lock 0 (buffer/len-chars lock))))

(defn buffer-content-lines []
  (with-read-lock [lock (buffer-text)]
    (let [num-lines (buffer/len-lines lock)]
      (mapv #(str/trim-newline (buffer/text-line lock %)) (range num-lines)))))

(defn buffer-display-content [buf]
  (str/strip-properties (buffer-content buf)))

(defn focused-buffer []
  (window-buffer (focused-window)))

(defn focused-buffer-name []
  (:name (focused-buffer)))

(defn focused-content []
  (buffer-content (focused-buffer)))

(defn focused-buffer-content []
  (focused-content))

(defn focused-display-content []
  (buffer-display-content (focused-buffer)))

(defn minibuffer-ui-content []
  (buffer-content (window-buffer (minibuffer-ui-window))))

(defn messages-content []
  (buffer-content (buffer/get-buffer "*Messages*")))

(defn find-line [content needle]
  (let [lines (str/split-lines content)]
    (some (fn [idx]
            (when (str/includes? (str (nth lines idx)) needle)
              idx))
      (range (count lines)))))

(defn move-to-line [buf win line]
  (move-cursor (with-read-lock [lock (buffer-text buf)]
                 (buffer/line-to-char lock line))
    false win))

(defn move-focused-line! [line]
  (let [win (focused-window)]
    (move-to-line (window-buffer win) win line)))

(defn move-focused-line-containing! [needle]
  (let [line (find-line (focused-display-content) needle)]
    (test/assert line
      (str "Could not find line containing " needle " in:\n" (focused-display-content)))
    (move-focused-line! line)
    line))

(defn window-for-buffer [buf]
  (first (filter #(= (:id (window-buffer %)) (:id buf))
           (frame-normal-windows))))

(defn buffer-line-text [buf line]
  (with-read-lock [lock (buffer-text buf)]
    (str/trim-newline (buffer/text-line lock line))))

(defn assert-focused-buffer-line [expected-line expected-text]
  (let [target-buffer (focused-buffer)]
    (is= expected-line (current-line target-buffer))
    (is= expected-text (buffer-line-text target-buffer expected-line))
    target-buffer))

(defn assert-focused-file-line [file-path expected-line expected-text]
  (let [target-buffer (focused-buffer)
        actual-file (:file target-buffer)]
    (test/assert actual-file
      (str "expected focused buffer to have a file path, got " (:name target-buffer)))
    (is= (path-canonicalize file-path) (path-canonicalize actual-file))
    (is= expected-line (current-line target-buffer))
    (is= expected-text (buffer-line-text target-buffer expected-line))
    target-buffer))

(defn command-window []
  (let [ui-win (minibuffer-ui-window)]
    (test/assert ui-win "regit command UI not open")
    ui-win))

(defn command-buffer []
  (window-buffer (command-window)))

(defn command-text []
  (buffer-display-content (command-buffer)))

(defn command-state []
  (let [ui-win (command-window)
        ui-buf (window-buffer ui-win)]
    (binding [*window* ui-win
              *buffer* ui-buf]
      @regit-command/*state*)))

(defn command-for [key]
  (let [ui-win (command-window)
        ui-buf (window-buffer ui-win)
        cmd (binding [*window* ui-win
                      *buffer* ui-buf]
              (regit-command/regit-command-keymap (keys/parse-key-sequence key)))]
    (when cmd
      (fn []
        (binding [*window* ui-win
                  *buffer* ui-buf]
          (cmd))))))

(defn invoke-command-key! [key]
  (let [cmd (command-for key)]
    (test/assert (ifn? cmd) (str "missing regit command key " key))
    (cmd)))

(defn invoke-status-key! [key]
  (let [win (focused-window)
        buf (window-buffer win)
        cmd (keys/lookup-keymap status/regit-status-keymap
              (keys/parse-key-sequence key))]
    (test/assert (ifn? cmd) (str "missing regit status key " key))
    (binding [*window* win
              *buffer* buf]
      (cmd))))

(defn close-command! []
  (when (minibuffer-ui-window)
    (when-let [cmd (command-for "q")]
      (cmd))))

(defn assert-command-contains [needle]
  (let [text (command-text)]
    (test/assert (str/includes? text needle)
      (str "expected regit command to contain " needle "\nGot:\n" text))))

(defn assert-buffer-face-at [buf text needle face]
  (let [pos (str/index-of text needle)]
    (test/assert pos (str "missing " needle " in " text))
    (binding [*buffer* buf]
      (let [props (buffer/property-at pos)]
        (test/assert (seq props) (str "missing properties at " needle))
        (is= (:fg (style->map (theme/style-for-face face)))
          (:fg (style->map (first props))))))))

(defn assert-buffer-color-at [buf text needle color]
  (let [pos (str/index-of text needle)]
    (test/assert pos (str "missing " needle " in " text))
    (binding [*buffer* buf]
      (let [props (buffer/property-at pos)]
        (test/assert (seq props) (str "missing properties at " needle))
        (is= (:fg (style->map (theme/color-style color)))
          (:fg (style->map (first props))))))))

(defn with-focused-window [f]
  (let [win (focused-window)
        buf (window-buffer win)]
    (binding [*window* win
              *buffer* buf]
      (f))))

(defn invoke-focused! [f]
  (with-focused-window f))

(defn select-current-iselect-entry! []
  (let [mb-win (minibuffer-window)]
    (test/assert mb-win "iselect minibuffer not opened")
    (binding [*window* mb-win
              *buffer* (window-buffer mb-win)]
      (iselect/select-current-entry))))

(defn set-iselect-input! [input]
  (let [mb-win (minibuffer-window)
        mb-buf (when mb-win (window-buffer mb-win))]
    (test/assert mb-buf "iselect minibuffer not opened")
    (binding [*window* mb-win
              *buffer* mb-buf]
      (set-string input mb-buf)
      (iselect/iselect-update-input))))

(defn iselect-state []
  (let [mb-win (minibuffer-window)
        mb-buf (when mb-win (window-buffer mb-win))]
    (test/assert mb-buf "iselect minibuffer not opened")
    (binding [*window* mb-win
              *buffer* mb-buf]
      @iselect/*state*)))

(defn invoke-iselect-key! [key]
  (let [mb-win (minibuffer-window)
        mb-buf (when mb-win (window-buffer mb-win))]
    (test/assert mb-buf "iselect minibuffer not opened")
    (binding [*window* mb-win
              *buffer* mb-buf]
      (let [cmd (keys/lookup-keymap iselect/iselect-keymap (keys/parse-key-sequence key))]
        (test/assert (ifn? cmd) (str "missing iselect key " key))
        (cmd)))))

(defn submit-simple-prompt! [input]
  (let [mb-win (minibuffer-window)]
    (test/assert mb-win "simple-prompt minibuffer not opened")
    (binding [*window* mb-win
              *buffer* (window-buffer mb-win)]
      (simple-prompt/set-input! input)
      (simple-prompt/simple-prompt-submit))))

(defn send-keys [keys-str]
  (let [frame-id (:id *frame*)]
    (frame/set-pending-sequence! frame-id [])
    (frame/set-numeric-prefix! frame-id nil))
  (doseq [keyspec (keys/parse-key-sequence keys-str)]
    (let [win (focused-window)]
      (binding [*buffer* (window-buffer win)
                *frame* (window-frame win)]
        (frame/process-key-event keyspec)))))

(defn status-content [root]
  (buffer-content (status/find-status-buffer root)))

(defn assert-focused-status [root context]
  (test/assert (= (status/find-status-buffer root) (focused-buffer))
    (str context ": expected focused regit-status buffer, got " (focused-buffer-name)))
  (test/assert (str/includes? (focused-content) "Repository:")
    (str context ": missing status repository heading"))
  (test/assert (str/includes? (focused-content) "Recent commits")
    (str context ": missing status recent commits")))

(defn wait-for-focused-status [root context]
  (test/wait-for [buf (focused-buffer)]
    :until (= (status/find-status-buffer root) buf)
    :timeout-message (fn []
                       (str context ": expected focused regit-status buffer, got "
                         (focused-buffer-name)))
    :return (assert-focused-status root context)))

(defn wait-for-message [needle context]
  (test/wait-for [content (messages-content)]
    :until (str/includes? content needle)
    :timeout-message (fn []
                       (str context ": expected message " needle ", got "
                         (messages-content)))))

(defn assert-focused-command [needle context]
  (test/assert (= (minibuffer-ui-window) (focused-window))
    (str context ": expected minibuffer UI focus, got " (focused-buffer-name)))
  (test/assert (str/includes? (minibuffer-ui-content) needle)
    (str context ": missing command text " needle)))

(defn assert-focused-regit-commit [context]
  (test/assert (str/includes? (focused-buffer-name) "regit-commit-message")
    (str context ": expected regit commit message buffer, got " (focused-buffer-name)))
  (binding [*buffer* (focused-buffer)]
    (is= :regit-commit-message *mode*)))

(defn assert-focused-commit-comments-dimmed [context]
  (let [content (focused-content)
        comment-pos (str/index-of content "#")]
    (test/assert comment-pos
      (str context ": expected git comments in commit message buffer. Got:\n" content))
    (binding [*buffer* (focused-buffer)]
      (test/assert (not (empty? (buffer/property-at comment-pos)))
        (str context ": expected comment text to have face properties")))))
