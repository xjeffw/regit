(ns regit.view-commit
  (:require [rex.base.mode :as mode]
            [rex.base.buffer :as buffer]
            [rex.base.frame :as frame]
            [rex.base.hook :refer [add-hook!]]
            [rex.base.window :as window]
            [rex.base.keys :as keys :refer [make-keymap map!]]
            [rex.base.project :as project]
            [rex.ui.outline :as outline]
            [rex.string :as str]
            [rex.base.theme :as theme]
            [regit.diff :as regit-diff]
            [rex.util :as util])
  (:use rex.core rex.builtins))

(defn- make-file-entry-items [entry]
  (regit-diff/make-file-entry-items entry {:hunk-id-section :new
                                           :include-entry? true}))

(defn- hash-text [text]
  (theme/with-face text :regit-hash))

(defn- repo-name [root]
  (or (path-filename root) root))

(defn- repo-text [text]
  (theme/with-face text :regit-repo))

(defn- view-commit-buffer-label [root commit-id]
  (str (theme/with-face "regit-commit: " :special-buffer)
    (repo-text (repo-name root))
    " "
    (hash-text commit-id)))

(defn- view-commit-buffer-name [commit-id]
  (str "*regit-commit: " commit-id "*"))

(defn- view-commit-preview-buffer-name [commit-id]
  (str "*regit-preview-commit: " commit-id "*"))

(defn- render-commit-heading [root commit-id info]
  (str
    (regit-diff/header-text "Commit:") " " (hash-text (:id info)) "\n"
    (regit-diff/header-text "Author:") " " (:author info) "\n"
    (regit-diff/header-text "Date:") " " (datetime (:date info)) "\n"
    "\n"
    (:summary info) "\n"
    (if (seq (:body info)) (str "\n" (:body info) "\n") "")))

(defn- render-commit-stat-line [line]
  (if (str/includes? line " | ")
    (let [parts (str/split line #" \| ")
          path (first parts)
          stat-str (str/join " | " (rest parts))]
      (str path " | "
        (str/join ""
          (map (fn [c]
                 (let [s (str c)]
                   (cond
                     (= s "+") (theme/with-color-style s :green)
                     (= s "-") (theme/with-color-style s :red)
                     :else s)))
            (seq stat-str)))))
    line))

(defn- render-commit-stat [stat]
  (let [lines (str/split-lines (or stat ""))]
    (str/join "\n" (map render-commit-stat-line lines))))

(defn- build-view-commit-tree-from-data [root commit-id info stat entries]
  [{:id :heading :text (render-commit-heading root commit-id info) :landmark [:heading]}
   {:id :stat :text (render-commit-stat stat) :landmark [:stat]}
   {:id :entries
    :text (regit-diff/header-text "Changes")
    :landmark [:entries]
    :children (let [children (mutable-vector)]
                (loop [remaining (seq entries)]
                  (if (seq remaining)
                    (let [e (first remaining)]
                      (conj children {:id [:file (:path e)]
                                      :text (regit-diff/entry-header-line e {:raw-kind? true})
                                      :landmark [:file (:path e)]
                                      :initially-expanded? false
                                      :entry e
                                      :leaf-hint-span? true
                                      :children-cache (atom nil)
                                      :deferred-children (fn [] (make-file-entry-items e))})
                      (recur (rest remaining)))
                    (vec children))))}])

(defn- build-view-commit-tree [root commit-id & [opts]]
  (let [opts (or opts {})
        info (if (contains? opts :commit-info)
               (:commit-info opts)
               (git-commit-info root commit-id))
        stat (if (contains? opts :commit-stat)
               (:commit-stat opts)
               (git-commit-stat root commit-id))
        entries (if (contains? opts :commit-diff)
                  (:commit-diff opts)
                  (git-commit-diff root commit-id))]
    (build-view-commit-tree-from-data root commit-id info stat entries)))

(defn- view-commit-file-ids [tree]
  (->> tree
    (filter #(= (:id %) :entries))
    first
    :children
    (map :id)))

(defn render-view-commit-buffer! [buffer root commit-id & [opts]]
  (let [tree (build-view-commit-tree root commit-id opts)
        expand-files? (not= false (:expand-files? opts))]
    (binding [*buffer* buffer]
      (regit-diff/configure-outline-buffer! buffer)
      (regit-diff/initialize-expanded-ids! buffer tree #{:heading :stat :entries}
        (fn [tree]
          (if expand-files?
            (view-commit-file-ids tree)
            [])))
      (outline/render-outline! buffer tree {:preserve-position? false}))))

(defn- find-view-commit-buffer [root commit-id]
  (let [name (view-commit-buffer-name commit-id)]
    (regit-diff/find-named-buffer name)))

(defn- configure-view-commit-buffer! [buffer root commit-id return-window preview? existing?]
  (let [state-before @(buffer-state buffer)
        already-rendered? (and existing?
                            (:outline-plan state-before)
                            (= (:regit-root state-before) root)
                            (= (:commit-id state-before) commit-id))]
    (binding [*buffer* buffer]
      (set-buffer-name (if preview?
                         (view-commit-preview-buffer-name commit-id)
                         (view-commit-buffer-name commit-id))
        buffer)
      (swap! (buffer-state buffer) assoc
        :project-root root
        :regit-root root
        :commit-id commit-id
        :label (view-commit-buffer-label root commit-id)
        :return-window return-window
        :regit-preview? preview?)
      (when-not existing?
        (mode/activate-mode :regit-view-commit))
      (when-not already-rendered?
        (render-view-commit-buffer! buffer root commit-id {:expand-files? (not preview?)}))
      buffer)))

(defn create-regit-view-commit-preview-buffer! [root commit-id return-window & [on-create]]
  (let [buffer (create-buffer true)]
    (when on-create
      (on-create buffer))
    (configure-view-commit-buffer! buffer root commit-id return-window true false)))

(defn promote-regit-view-commit-preview! [buffer return-window]
  (let [state @(buffer-state buffer)
        root (:regit-root state)
        commit-id (:commit-id state)
        was-preview? (:regit-preview? state)]
    (set-buffer-name (view-commit-buffer-name commit-id) buffer)
    (swap! (buffer-state buffer) assoc
      :project-root root
      :regit-root root
      :commit-id commit-id
      :label (view-commit-buffer-label root commit-id)
      :return-window return-window
      :regit-preview? false)
    (when was-preview?
      (swap! (buffer-state buffer) dissoc :expanded-ids :seen-ids)
      (render-view-commit-buffer! buffer root commit-id {:expand-files? true}))
    buffer))

(defn ^:interactive regit-view-commit [commit-id]
  (let [return-window (focused-window)
        root (or (:regit-root *buffer*) (current-directory))
        existing (find-view-commit-buffer root commit-id)
        buffer (or existing (create-buffer true))
        window (or (regit-diff/regit-view-window {:source-window return-window
                                                  :fallback-to-created-window? true})
                 (focused-window))]
    (frame/with-render-coalescing
      (set-window-buffer buffer window)
      (set-focused-window window)
      (binding [*buffer* buffer
                *window* window]
        (configure-view-commit-buffer! buffer root commit-id return-window false existing)
        (move-cursor 0 false window)
        (set-scroll-offset 0 window)
        (when (= (:id (window-buffer window)) (:id buffer))
          (outline/update-outline-hint-highlight!))
        buffer))))

(defn ^:interactive regit-view-commit-quit []
  (regit-diff/close-buffer-returning-to-window!))

(defn- short-commit-id [commit-id]
  (subs commit-id 0 (min 7 (count commit-id))))

(defn- open-synthetic-file! [root path ref name]
  (try
    (let [content (git-show root (str ref ":" path))
          tmp-path (temp-file-path (path-filename path))
          _ (write-file tmp-path content)
          buffer (open-file tmp-path)]
      (set-buffer-name name buffer)
      (set-buffer-read-only true buffer)
      (binding [*buffer* buffer]
        (ignore-errors
          (mode/enable-submode :regit-synthetic)))
      buffer)
    (catch exception e
      (message (str "Failed to open synthetic file: " (ex-message e)))
      nil)))

(defn- view-commit-section-id? [id]
  (and (>= (count id) 3)
    (= (second id) :commit)))

(defn- view-commit-file-path [id]
  (if (view-commit-section-id? id)
    (nth id 2)
    (second id)))

(defn- view-commit-hunk-id [id]
  (when (= (first id) :hunk)
    (if (view-commit-section-id? id)
      (when (>= (count id) 4) (nth id 3))
      (when (>= (count id) 3) (nth id 2)))))

(defn- view-commit-line-offset [id]
  (if (and (= (first id) :hunk) (view-commit-section-id? id))
    (if (>= (count id) 5) (nth id 4) 0)
    (if (and (= (first id) :hunk) (>= (count id) 4)) (nth id 3) 0)))

(defn- open-view-commit-hunk-target-buffer [root commit-id file-path full-path entry force-file? target-type]
  (cond
    force-file?
    (open-file full-path)

    (= target-type :old)
    (let [old-path (or (:old-path entry) file-path)]
      (open-synthetic-file! root old-path (str commit-id "^")
        (str "*commit[" (short-commit-id commit-id) "^]: " old-path "*")))

    :else
    (open-synthetic-file! root file-path commit-id
      (str "*commit[" (short-commit-id commit-id) "]: " file-path "*"))))

(defn- open-view-commit-file-target! [root commit-id item id force-file?]
  (let [kind (first id)
        file-path (view-commit-file-path id)
        full-path (path-join root file-path)
        entry (:entry item)
        hunk-id (view-commit-hunk-id id)
        line-offset (view-commit-line-offset id)
        hunk (when (and entry hunk-id)
               (regit-diff/find-hunk entry :new hunk-id))
        hunk-info (if hunk
                    (regit-diff/parse-hunk-header (first hunk))
                    {:old-start 1 :new-start 1})
        line-text (:text item)]
    (if (= kind :file)
      (regit-diff/open-working-file-at-line! full-path
        (dec (:new-start hunk-info)))
      (let [target-type (regit-diff/hunk-target-type line-text force-file?)
            line (regit-diff/hunk-target-line hunk hunk-info line-offset target-type)
            buffer (open-view-commit-hunk-target-buffer root commit-id file-path full-path entry force-file? target-type)]
        (when buffer
          (regit-diff/show-buffer-at-line! buffer line))))))

(defn- jump-to-view-commit-target! [force-file?]
  (let [focused (focused-window)
        dynamic-window-buffer (when *window* (ignore-errors (window-buffer *window*)))
        window (if (and *window*
                     *buffer*
                     dynamic-window-buffer
                     (= (:id *buffer*) (:id dynamic-window-buffer)))
                 *window*
                 focused)
        window-buffer (when window (window-buffer window))
        buffer (if (and *buffer*
                     window-buffer
                     (= (:id *buffer*) (:id window-buffer)))
                 *buffer*
                 (or window-buffer *buffer*))]
    (binding [*buffer* buffer
              *window* window]
      (let [state @(buffer-state buffer)
            line (current-line)
            item (get (:line-to-item state) line)
            id (:id item)
            root (:regit-root state)
            commit-id (:commit-id state)]
        (if (and root
              commit-id
              (vector? id)
              (or (= (first id) :file) (= (first id) :hunk)))
          (open-view-commit-file-target! root commit-id item id force-file?)
          (message "No regit action available at this line"))))))

(defn ^:interactive regit-view-commit-enter []
  (jump-to-view-commit-target! false))

(defn ^:interactive regit-view-commit-jump-to-file []
  (jump-to-view-commit-target! true))

(def regit-view-commit-keymap (make-keymap))

(map! :map regit-view-commit-keymap
  ("q" #'regit-view-commit-quit)
  ("<enter>" #'regit-view-commit-enter)
  ("C-<enter>" #'regit-view-commit-jump-to-file))

(def regit-view-commit-keymaps
  [regit-view-commit-keymap])

(mode/register-mode :regit-view-commit
  {:name :regit-view-commit
   :icon "󰊢 "
   :keymaps regit-view-commit-keymaps
   :submodes [:outline :vim]})
