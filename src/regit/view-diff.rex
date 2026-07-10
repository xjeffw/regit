(ns regit.view-diff
  (:require [rex.base.mode :as mode]
            [rex.base.buffer :as buffer]
            [rex.base.frame :as frame]
            [rex.base.window :as window]
            [rex.base.keys :as keys :refer [make-keymap map!]]
            [rex.base.project :as project]
            [rex.ui.outline :as outline]
            [rex.string :as str]
            [rex.base.theme :as theme]
            [regit.diff :as regit-diff]
            [rex.util :as util])
  (:use rex.core rex.builtins))

(defn- repo-name [root]
  (or (path-filename root) root))

(defn- normalize-section [section]
  (case section
    :unstaged :unstaged
    :staged))

(defn- view-diff-buffer-name [root & [section]]
  (let [section (normalize-section section)]
    (if (= section :staged)
      (str "*regit-diff: " (repo-name root) "*")
      (str "*regit-diff unstaged: " (repo-name root) "*"))))

(defn- section-title [section]
  (case section
    :unstaged "Unstaged changes"
    "Staged changes"))

(defn- heading-title [section]
  (case section
    :unstaged "Working tree diff"
    "Pending commit"))

(defn- diff-entries [root section]
  (case section
    :unstaged (let [{:keys [unstaged]} (git-status root)]
                (filterv #(not= (:kind %) :untracked) unstaged))
    (git-staged-diff root)))

(defn- make-file-entry-items [section entry]
  (regit-diff/make-file-entry-items entry {:hunk-id-section (if (= section :unstaged) :new :old)
                                           :include-entry? true}))

(defn- build-view-diff-tree [root section]
  (let [entries (diff-entries root section)]
    [{:id :heading
      :text (str
              (regit-diff/header-text (heading-title section))
              "\n"
              (regit-diff/header-text "Root:")
              " "
              root)
      :landmark [:heading]}
     {:id :summary
      :text (regit-diff/render-summary entries)
      :landmark [:summary]}
     {:id :entries
      :text (regit-diff/header-text (str (section-title section) " (" (count entries) ")"))
      :landmark [:entries]
      :children (if (seq entries)
                  (mapv (fn [e]
                          {:id [:file (:path e)]
                           :text (regit-diff/entry-header-line e)
                           :landmark [:file (:path e)]
                           :initially-expanded? false
                           :entry e
                           :leaf-hint-span? true
                           :children-cache (atom nil)
                           :deferred-children (fn [] (make-file-entry-items section e))})
                    entries)
                  [{:id :no-changes :text (str "  (no " (str/lower-case (section-title section)) ")")}])}]))

(defn- view-diff-file-ids [tree]
  (->> tree
    (filter #(= (:id %) :entries))
    first
    :children
    (map :id)))

(defn- render-view-diff-buffer! [buffer root section]
  (let [tree (build-view-diff-tree root section)]
    (binding [*buffer* buffer]
      (regit-diff/configure-outline-buffer! buffer)
      (regit-diff/initialize-expanded-ids! buffer tree #{:heading :summary :entries}
        view-diff-file-ids)
      (outline/render-outline! buffer tree))))

(defn- find-view-diff-buffer [root section]
  (let [name (view-diff-buffer-name root section)]
    (regit-diff/find-named-buffer name)))

(defn ^:interactive regit-view-diff [& [root section]]
  (let [return-window (focused-window)
        root (or root (:regit-root *buffer*) (current-directory))
        section (normalize-section section)
        existing (find-view-diff-buffer root section)
        buffer (or existing (create-buffer true))
        window (or (regit-diff/regit-view-window {:fallback-to-created-window? true}) (focused-window))]
    (frame/with-render-coalescing
      (set-window-buffer buffer window)
      (set-focused-window window)
      (binding [*buffer* buffer
                *window* window]
        (set-buffer-name (view-diff-buffer-name root section) buffer)
        (swap! (buffer-state buffer) assoc
          :project-root root
          :regit-root root
          :regit-diff-section section
          :return-window return-window)
        (when-not existing
          (mode/activate-mode :regit-view-diff))
        (render-view-diff-buffer! buffer root section)
        (move-cursor 0 false window)
        (set-scroll-offset 0 window)
        (outline/update-outline-hint-highlight!)
        buffer))))

(defn ^:interactive regit-view-diff-quit []
  (regit-diff/close-buffer-returning-to-window!))

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
    (catch e
      (message (str "Failed to open synthetic file: " (ex-message e)))
      nil)))

(defn- view-diff-hunk-id-section [section]
  (if (= section :unstaged) :new :old))

(defn- open-view-diff-hunk-target-buffer [root section file-path full-path entry force-file? target-type]
  (cond
    force-file?
    (open-file full-path)

    (= target-type :old)
    (let [old-path (or (:old-path entry) file-path)]
      (if (= section :staged)
        (open-synthetic-file! root old-path "HEAD" (str "*staged-base: " old-path "*"))
        (open-synthetic-file! root file-path "" (str "*staged: " file-path "*"))))

    (= section :staged)
    (open-synthetic-file! root file-path "" (str "*staged: " file-path "*"))

    :else
    (open-file full-path)))

(defn- open-view-diff-file-target! [root section item id force-file?]
  (let [kind (first id)
        file-path (second id)
        full-path (path-join root file-path)
        entry (:entry item)
        hunk-id (when (and (= kind :hunk) (>= (count id) 3)) (nth id 2))
        line-offset (if (and (= kind :hunk) (>= (count id) 4)) (nth id 3) 0)
        hunk-id-section (view-diff-hunk-id-section section)
        hunk (when (and entry hunk-id)
               (regit-diff/find-hunk entry hunk-id-section hunk-id))
        hunk-info (if hunk
                    (regit-diff/parse-hunk-header (first hunk))
                    {:old-start 1 :new-start 1})
        line-text (:text item)]
    (if (= kind :file)
      (regit-diff/open-working-file-at-line! full-path
        (dec (:new-start hunk-info)))
      (let [target-type (regit-diff/hunk-target-type line-text force-file?)
            line (regit-diff/hunk-target-line hunk hunk-info line-offset target-type)
            buffer (open-view-diff-hunk-target-buffer root section file-path full-path entry force-file? target-type)]
        (when buffer
          (regit-diff/show-buffer-at-line! buffer line))))))

(defn- jump-to-view-diff-target! [force-file?]
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
            section (:regit-diff-section state)]
        (if (and root
              (vector? id)
              (or (= (first id) :file) (= (first id) :hunk)))
          (open-view-diff-file-target! root section item id force-file?)
          (message "No regit action available at this line"))))))

(defn ^:interactive regit-view-diff-enter []
  (jump-to-view-diff-target! false))

(defn ^:interactive regit-view-diff-jump-to-file []
  (jump-to-view-diff-target! true))

(def regit-view-diff-keymap (make-keymap))

(map! :map regit-view-diff-keymap
  ("q" #'regit-view-diff-quit)
  ("<enter>" #'regit-view-diff-enter)
  ("C-<enter>" #'regit-view-diff-jump-to-file))

(def regit-view-diff-keymaps
  [regit-view-diff-keymap])

(mode/register-mode :regit-view-diff
  {:name :regit-view-diff
   :icon "󰊢 "
   :keymaps regit-view-diff-keymaps
   :submodes [:outline :vim]})
