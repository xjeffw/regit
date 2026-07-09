(ns regit.tests.remote
  (:require [regit.remote :as remote]
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

(defn- git-config-all [root key]
  (let [{:keys [code out]} (git root "config" "--get-all" key)]
    (if (zero? code)
      (vec (remove str/blank? (str/split-lines out)))
      [])))

(defn- git-set-all! [root key values]
  (git root "config" "--unset-all" key)
  (doseq [value values]
    (git! root "config" "--add" key value)))

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

(defn- init-master-test-repo [name]
  (let [tmp-dir (temp-file-path name)
        _ (run-shell* "rm" ["-rf" tmp-dir])
        _ (run-shell* "mkdir" [tmp-dir])
        _ (git! tmp-dir "init")
        _ (git! tmp-dir "config" "user.email" "test@example.com")
        _ (git! tmp-dir "config" "user.name" "Test User")
        _ (commit-file! tmp-dir "test.txt" "hello\n" "initial")
        _ (git! tmp-dir "branch" "-M" "master")]
    tmp-dir))

(defn- init-bare-repo [name]
  (let [tmp-dir (temp-file-path name)
        _ (run-shell* "rm" ["-rf" tmp-dir])
        _ (run-shell* "mkdir" [tmp-dir])
        result (run-shell* "git" ["init" "--bare" tmp-dir])]
    (test/assert (zero? (:code result))
      (str "git init --bare failed: " (:err result) (:out result)))
    tmp-dir))

(defn- cleanup [& paths]
  (doseq [path paths]
    (when path
      (run-shell* "rm" ["-rf" path]))))

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
    (test/assert (ifn? cmd) (str "missing regit remote command key " key))
    (let [ui-buf (command-buffer)]
      (binding [*buffer* ui-buf]
        (cmd)))))

(defn- close-command! []
  (when-let [ui-win (minibuffer-ui-window)]
    (let [ui-buf (window-buffer ui-win)]
      (binding [*buffer* ui-buf]
        (when-let [cmd (regit-command/regit-command-keymap (keys/parse-key-sequence "q"))]
          (cmd))))))

(defn- assert-command-contains [needle]
  (let [text (command-text)]
    (test/assert (str/includes? text needle)
      (str "expected remote command to contain " needle "\nGot:\n" text))))

(defn- set-iselect-input! [input]
  (let [mb-win (minibuffer-window)
        mb-buf (when mb-win (window-buffer mb-win))]
    (test/assert mb-buf "iselect minibuffer not opened")
    (binding [*window* mb-win
              *buffer* mb-buf]
      (set-string input mb-buf)
      (iselect/iselect-update-input))))

(defn- select-current-iselect-entry! []
  (let [mb-win (minibuffer-window)]
    (test/assert mb-win "iselect minibuffer not opened")
    (binding [*buffer* (window-buffer mb-win)]
      (iselect/select-current-entry))))

(defn- submit-simple-prompt! [input]
  (let [mb-win (minibuffer-window)]
    (test/assert mb-win "simple-prompt minibuffer not opened")
    (binding [*buffer* (window-buffer mb-win)]
      (simple-prompt/set-input! input)
      (simple-prompt/simple-prompt-submit))))

(defn- ref-exists? [root ref]
  (zero? (:code (git root "show-ref" "--verify" "--quiet" ref))))

(deftest regit-remote-command-layout-and-status-binding-test
  (theme/load-theme :catppuccin-frappe)
  (let [tmp-dir (init-test-repo "regit-remote-command")
        _ (git! tmp-dir "remote" "add" "origin" "git@example.com:xjefw/rex.git")]
    (remote/regit-remote tmp-dir)
    (let [st (command-state)]
      (test/assert st "regit-command state not initialized")
      (is= "- f" (get-in st [:args :fetch-after-add :key]))
      (is= true (get-in st [:args :fetch-after-add :value]))
      (doseq [key ["u" "U" "s" "S" "O" "a" "r" "k" "C" "p" "P" "z" "d u"]]
        (test/assert (contains? (:actions st) key) (str "missing remote action " key))))
    (doseq [needle ["Variables"
                    "remote.origin.url"
                    "remote.origin.fetch"
                    "remote.origin.pushurl"
                    "remote.origin.push"
                    "remote.origin.tagOpt"
                    "Arguments for add"
                    "-f Fetch after add (-f)"
                    "Actions"
                    "Add"
                    "Rename"
                    "Remove"
                    "Configure..."
                    "Prune stale branches"
                    "Prune stale refspecs"
                    "Update default branch"]]
      (assert-command-contains needle))
    (close-command!)
    (status/regit-status tmp-dir)
    (let [cmd (keys/lookup-keymap status/regit-status-keymap (keys/parse-key-sequence "M"))]
      (test/assert (ifn? cmd) "regit-status did not bind M to remote"))
    (cleanup tmp-dir)))

(deftest regit-remote-configure-variables-test
  (let [tmp-dir (init-test-repo "regit-remote-configure")
        _ (git! tmp-dir "remote" "add" "origin" "git@example.com:xjefw/rex.git")]
    (remote/regit-remote tmp-dir)
    (invoke-command-key! "C")
    (select-current-iselect-entry!)
    (assert-command-contains "Configure origin")

    (invoke-command-key! "O")
    (is= "--no-tags" (git-config tmp-dir "remote.origin.tagOpt"))
    (assert-command-contains "remote.origin.tagOpt")
    (invoke-command-key! "O")
    (is= "--tags" (git-config tmp-dir "remote.origin.tagOpt"))
    (invoke-command-key! "O")
    (is= nil (git-config tmp-dir "remote.origin.tagOpt"))

    (invoke-command-key! "u")
    (submit-simple-prompt! "ssh://example/rex.git")
    (is= ["ssh://example/rex.git"] (git-config-all tmp-dir "remote.origin.url"))
    (assert-command-contains "ssh://example/rex.git")

    (invoke-command-key! "U")
    (submit-simple-prompt! "+refs/heads/main:refs/remotes/origin/main")
    (is= ["+refs/heads/main:refs/remotes/origin/main"]
      (git-config-all tmp-dir "remote.origin.fetch"))

    (invoke-command-key! "s")
    (submit-simple-prompt! "ssh://push.example/rex.git")
    (is= ["ssh://push.example/rex.git"] (git-config-all tmp-dir "remote.origin.pushurl"))

    (invoke-command-key! "S")
    (submit-simple-prompt! "refs/heads/main:refs/heads/main")
    (is= "refs/heads/main:refs/heads/main" (git-config tmp-dir "remote.origin.push"))

    (close-command!)
    (cleanup tmp-dir)))

(deftest regit-remote-add-rename-remove-test
  (let [tmp-dir (init-test-repo "regit-remote-add-rename-remove")]
    (remote/regit-remote tmp-dir)
    (invoke-command-key! "a")
    (submit-simple-prompt! "fork")
    (submit-simple-prompt! tmp-dir)
    (submit-simple-prompt! "y")
    (is= tmp-dir (git-out tmp-dir "remote" "get-url" "fork"))
    (is= "fork" (git-config tmp-dir "remote.pushDefault"))

    (git! tmp-dir "config" "branch.main.pushRemote" "fork")
    (let [err (remote/rename-remote! tmp-dir "fork" "upstream")]
      (test/assert (not err) (str "rename remote failed: " err)))
    (is= tmp-dir (git-out tmp-dir "remote" "get-url" "upstream"))
    (is= "upstream" (git-config tmp-dir "remote.pushDefault"))
    (is= "upstream" (git-config tmp-dir "branch.main.pushRemote"))

    (let [err (remote/remove-remote! tmp-dir "upstream")]
      (test/assert (not err) (str "remove remote failed: " err)))
    (is= nil (git-config tmp-dir "remote.pushDefault"))
    (is= nil (git-config tmp-dir "branch.main.pushRemote"))
    (cleanup tmp-dir)))

(deftest regit-remote-prune-stale-refspecs-test
  (let [tmp-dir (init-test-repo "regit-remote-prune-refspecs")
        _ (git! tmp-dir "remote" "add" "origin" tmp-dir)]
    (git-set-all! tmp-dir "remote.origin.fetch"
      ["+refs/missing/*:refs/remotes/origin/*"])
    (let [err (remote/prune-stale-refspecs! tmp-dir "origin" :default)]
      (test/assert (not err) (str "replace stale refspec failed: " err)))
    (is= ["+refs/heads/*:refs/remotes/origin/*"]
      (git-config-all tmp-dir "remote.origin.fetch"))

    (git! tmp-dir "update-ref" "refs/remotes/origin/tags/v1" "HEAD")
    (git-set-all! tmp-dir "remote.origin.fetch"
      ["+refs/heads/*:refs/remotes/origin/*"
       "+refs/tags/*:refs/remotes/origin/tags/*"])
    (let [err (remote/prune-stale-refspecs! tmp-dir "origin" :prune)]
      (test/assert (not err) (str "prune stale refspec failed: " err)))
    (is= ["+refs/heads/*:refs/remotes/origin/*"]
      (git-config-all tmp-dir "remote.origin.fetch"))
    (test/assert (not (ref-exists? tmp-dir "refs/remotes/origin/tags/v1"))
      "stale tracking ref should have been deleted")
    (cleanup tmp-dir)))

(deftest regit-remote-update-default-branch-test
  (let [tmp-dir (init-master-test-repo "regit-remote-update-default")
        bare-dir (init-bare-repo "regit-remote-update-default-bare")]
    (git! tmp-dir "remote" "add" "origin" bare-dir)
    (git! tmp-dir "push" "-u" "origin" "master")
    (git! tmp-dir "remote" "set-head" "origin" "master")
    (let [head (git-out tmp-dir "rev-parse" "HEAD")]
      (let [result (run-shell* "git" ["--git-dir" bare-dir "update-ref" "refs/heads/main" head])]
        (test/assert (zero? (:code result))
          (str "bare update-ref failed: " (:err result) (:out result))))
      (let [result (run-shell* "git" ["--git-dir" bare-dir "symbolic-ref" "HEAD" "refs/heads/main"])]
        (test/assert (zero? (:code result))
          (str "bare symbolic-ref failed: " (:err result) (:out result)))))
    (remote/update-default-branch! tmp-dir (focused-window))
    (submit-simple-prompt! "y")
    (is= "main" (git-out tmp-dir "rev-parse" "--abbrev-ref" "HEAD"))
    (is= "origin/main" (git-out tmp-dir "rev-parse" "--abbrev-ref" "--symbolic-full-name" "main@{upstream}"))
    (cleanup tmp-dir bare-dir)))
