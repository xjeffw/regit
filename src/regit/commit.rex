(ns regit.commit
  (:require [regit.command :as regit-command :refer [regit-command]]
            [regit.view-diff :refer [regit-view-diff]]
            [regit.util :as regit-util]
            [rex.base.hook :refer [run-hooks]]
            [rex.string :as str]
            [rex.base.project :as project]
            [rex.base.keys :as keys :refer [make-keymap map!]]
            [rex.base.mode :refer [register-mode activate-mode]]
            [rex.base.buffer :as buffer])
  (:use rex.core rex.builtins))

(defn- result-details [result]
  (->> [(:err result) (:out result)]
    (map str/trim)
    (remove str/blank?)
    (str/join "\n")))

(defn- git-error-message [operation result]
  (let [details (result-details result)]
    (if (str/blank? details)
      (str operation " failed (exit " (:code result) ")")
      (str operation " failed: " details))))

(defn- notify-project-file-git-change! [root]
  (doseq [buf (project/project-file-buffers root)]
    (when-let [file (:file buf)]
      (binding [*buffer* buf]
        (run-hooks buffer/on-buffer-file-git-change file)))))

(defn- refresh-status! [root start-window]
  (when (call-var regit.status/find-status-buffer root)
    (let [should-focus? (if start-window (= (focused-window) start-window) nil)]
      (or (call-var regit.status/refresh-status! root should-focus?)
        (call-var regit.status/regit-status root should-focus?)))))

(defn- after-git-change! [root start-window]
  (git-refresh-index root)
  (git-clear-repo-cache)
  (notify-project-file-git-change! root)
  (refresh-status! root start-window))

(defn- run-git-result! [root operation result opts]
  (let [start-window (:start-window opts)
        success-message (:success-message opts)
        success? (zero? (:code result))]
    (after-git-change! root start-window)
    (if success?
      (do
        (when-not (str/blank? (or success-message ""))
          (message success-message))
        nil)
      (let [msg (git-error-message operation result)]
        (message msg)
        msg))))

(defn- run-git-with-commit-editor! [root operation env args & [opts]]
  (let [opts (or opts {})]
    (regit-util/run-git-with-commit-editor! root operation env args
      (assoc opts
        :on-result (fn [result]
                     (run-git-result! root operation result opts))
        :on-abort (fn []
                    (after-git-change! root (:start-window opts)))))))

(defn- buffer-string [& [buf]]
  (let [buf (or buf *buffer*)]
    (with-read-lock [lock (buffer-text buf)]
      (buffer/slice lock 0 (buffer/len-chars lock)))))

(declare finish-commit abort-commit)

(def commit-message-static-keymap (make-keymap))

(defn- commit-message-keymap [key-seq]
  (keys/lookup-keymap commit-message-static-keymap key-seq))

(register-mode :regit-commit-message
  {:name :regit-commit-message
   :keymaps [#'commit-message-keymap]
   :submodes [:vim]})

(defn- kill-associated-view-diff-buffer! [commit-buffer]
  (let [state @(buffer-state commit-buffer)
        diff-buffer (:regit-view-diff-buffer state)]
    (when diff-buffer
      (ignore-errors
        (kill-buffer diff-buffer)))))

(defn- close-commit-buffer! [buffer return-window]
  (close-buffer true buffer)
  (when return-window
    (set-focused-window return-window)))

(defn- editor-backed-commit-message? [state]
  (:regit-commit-editor-state state))

(defn- finish-git-editor-commit-message! [buffer state]
  (let [state-atom (:regit-commit-editor-state state)
        editor-state @state-atom
        message-path (:regit-commit-message-path state)
        return-window (:return-window editor-state)]
    (write-file message-path (buffer-string buffer))
    (write-file (:continue-path editor-state) "")
    (swap! state-atom assoc :active-buffer nil)
    (kill-associated-view-diff-buffer! buffer)
    (close-commit-buffer! buffer return-window)))

(defn finish-commit []
  (let [state @(buffer-state *buffer*)]
    (if (editor-backed-commit-message? state)
      (finish-git-editor-commit-message! *buffer* state)
      (message "No git commit editor is active"))))

(defn ^:interactive abort-commit []
  (let [buffer *buffer*
        state @(buffer-state buffer)]
    (if (editor-backed-commit-message? state)
      (let [state-atom (:regit-commit-editor-state state)
            editor-state @state-atom
            return-window (:return-window editor-state)]
        (write-file (:abort-path editor-state) "")
        (swap! state-atom assoc :active-buffer nil)
        (kill-associated-view-diff-buffer! buffer)
        (close-commit-buffer! buffer return-window))
      (do
        (kill-associated-view-diff-buffer! buffer)
        (close-buffer)))))

(map! :map commit-message-static-keymap
  ("C-c C-c" #'finish-commit)
  ("C-c C-k" #'abort-commit))

(defn- open-staged-diff-for-commit-buffer! [root commit-buffer]
  (let [commit-window (focused-window)
        diff-buffer (regit-view-diff root)]
    (swap! (buffer-state commit-buffer) assoc
      :regit-view-diff-buffer diff-buffer)
    (when commit-window
      (set-window-buffer commit-buffer commit-window)
      (set-focused-window commit-window)
      (binding [*buffer* commit-buffer
                *window* commit-window]
        (move-cursor 0 false commit-window)
        (set-scroll-offset 0 commit-window)))))

(defn- commit-args [amend?]
  (cond-> ["commit"]
    amend? (conj "--amend")))

(defn- run-commit-with-editor! [root amend? return-window]
  (let [operation (if amend? "Amend" "Commit")]
    (run-git-with-commit-editor! root operation {} (commit-args amend?)
      {:start-window return-window
       :return-window return-window
       :success-message (if amend? "Amended" "Committed")
       :abort-message "Aborted commit message edit"
       :on-open-editor (fn [buffer]
                         (open-staged-diff-for-commit-buffer! root buffer))})))

(defn ^:interactive regit-commit []
  (if-let [root (project/current-project-root)]
    (let [return-window (focused-window)]
      (regit-command
        {:args {}
         :return-window return-window
         :actions {"c" {:label "Commit"
                        :fn (fn [_] (run-commit-with-editor! root false return-window))}
                   "a" {:label "Amend"
                        :fn (fn [_] (run-commit-with-editor! root true return-window))}}
         :layout [{:section "Actions"
                   :columns 3
                   :items [{:action "c"}
                           {:action "a"}]}]}))
    (message "Not in a git repository")))
