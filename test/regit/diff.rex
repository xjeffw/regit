(ns regit.tests.diff
  (:require [regit.diff :as diff]
            [regit.tests.util :refer [buffer-content find-line git! git-output move-to-line]]
            [rex.string :as str]
            [rex.test :as test :refer [deftest is= is-error]]
            [rex.base.buffer :as buffer]
            [regit.status :as status]
            [regit.view-commit :as view-commit]
            [regit.view-diff :as view-diff]
            [regit.view-stash :as view-stash]
            [rex.ui.outline :as outline]))
(deftest entry-header-line-file-status-face-test
  (let [line (diff/entry-header-line {:kind :modified :path "tracked.txt"})]
    (test/assert (property-string? line) "entry header should preserve styled file status")
    (test/assert (str/includes? line "modified tracked.txt") "entry header missing text")))

(deftest scaled-summary-widths-test
  (is= {:added 60 :removed 40} (diff/scaled-summary-widths 60 40 100))
  (is= {:added 75 :removed 25} (diff/scaled-summary-widths 150 50 200))
  (is= {:added 15 :removed 5} (diff/scaled-summary-widths 30 10 200))
  (is= {:added 1 :removed 99} (diff/scaled-summary-widths 1 999 1000))
  (is= {:added 1 :removed 0} (diff/scaled-summary-widths 1 0 1000))
  (is= {:added 0 :removed 1} (diff/scaled-summary-widths 0 1 1000)))

(defn- count-char [s ch]
  (reduce (fn [n c] (if (= c ch) (inc n) n)) 0 (seq (str s))))

(deftest render-summary-scales-bars-from-largest-file-test
  (let [entry {:path "big.rex"
               :diff (str (repeat-str 130 "+added\n")
                       (repeat-str 65 "-removed\n"))}
        summary (diff/render-summary [entry])
        line (second (str/split-lines summary))]
    (is= 100 (+ (count-char line \+) (count-char line \-)))))

(defn- repo-with-change [name]
  (let [root (temp-file-path name)
        file-path (path-join root "test.txt")]
    (run-shell* "rm" ["-rf" root] {:direnv false})
    (run-shell* "mkdir" [root] {:direnv false})
    (git! root "init")
    (git! root "config" "user.name" "Rex Test")
    (git! root "config" "user.email" "rex@example.com")
    (write-file file-path "line 1\nline 2\n")
    (git! root "add" "test.txt")
    (git! root "commit" "-m" "initial")
    (write-file file-path "line 1 changed\nline 2\n")
    root))

(defn- repo-with-committed-change [name]
  (let [root (repo-with-change name)]
    (git! root "add" "test.txt")
    (git! root "commit" "-m" "second")
    root))

(defn- repo-with-stash [name]
  (let [root (repo-with-change name)]
    (git! root "add" "test.txt")
    (git! root "stash" "push" "-m" "context stash")
    root))

(defn- diff-context-decoration [buffer]
  (diff/regit-diff-context-decoration-for-buffer buffer))

(defn- diff-context-decoration-size [buffer]
  (:size (diff-context-decoration buffer)))

(defn- diff-context-row-at-line [buffer line]
  (binding [*buffer* buffer]
    (let [decoration (diff-context-decoration buffer)
          rendered (when decoration
                     (buffer-decoration-text decoration 80 1 (inc line) buffer))]
      (when rendered
        (nth rendered 0)))))

(defn- diff-context-row-text [row]
  (let [text (or (:text row) "")]
    (if (property-string? text)
      (str/strip-properties text)
      text)))

(defn- assert-diff-context-renders-hunk-file-header [buffer window diff/header-text content-needle]
  (let [decoration (diff-context-decoration buffer)
        text (buffer-content buffer)
        file-line (find-line text diff/header-text)
        hunk-line (find-line text "@@")
        content-line (find-line text content-needle)]
    (test/assert decoration
      (str "regit diff outline buffer should install diff context decoration; decorations="
        (pr-str (buffer-decorations buffer))))
    (is= :top (:side decoration))
    (is= 0 (:size decoration))
    (test/assert file-line "expected file header line")
    (test/assert hunk-line "expected hunk header line")
    (test/assert content-line "expected hunk content line")
    (set-scroll-offset hunk-line window)
    (is= 1 (diff-context-decoration-size buffer))
    (set-scroll-offset file-line window)
    (is= 0 (diff-context-decoration-size buffer))
    (is= diff/header-text
      (diff-context-row-text (diff-context-row-at-line buffer hunk-line)))
    (is= diff/header-text
      (diff-context-row-text (diff-context-row-at-line buffer content-line)))))

(deftest regit-view-diff-context-decoration-renders-hunk-file-header-test
  (let [root (repo-with-change "regit-view-diff-context-decoration-test")]
    (try
      (delete-other-windows)
      (let [buffer (view-diff/regit-view-diff root :unstaged)
            window (focused-window)
            text (buffer-content buffer)
            file-line (find-line text "modified test.txt")
            hunk-line (find-line text "@@")
            content-line (find-line text "+line 1 changed")
            decoration (diff-context-decoration buffer)]
        (test/assert decoration
          (str "regit diff buffers should install diff context decoration; decorations="
            (pr-str (buffer-decorations buffer))))
        (is= :top (:side decoration))
        (is= 0 (:size decoration))
        (test/assert file-line "expected file header line")
        (test/assert hunk-line "expected hunk header line")
        (test/assert content-line "expected hunk content line")
        (set-scroll-offset hunk-line window)
        (is= 1 (diff-context-decoration-size buffer))
        (set-scroll-offset file-line window)
        (is= 0 (diff-context-decoration-size buffer))
        (let [header-row (diff-context-row-at-line buffer hunk-line)
              content-row (diff-context-row-at-line buffer content-line)
              file-row (diff-context-row-at-line buffer file-line)]
          (is= "modified test.txt" (diff-context-row-text header-row))
          (is= "modified test.txt" (diff-context-row-text content-row))
          (is= "" (diff-context-row-text file-row))))
      (finally
        (run-shell* "rm" ["-rf" root] {:direnv false})))))

(deftest regit-status-diff-context-decoration-renders-expanded-hunk-test
  (let [root (repo-with-change "regit-status-diff-context-decoration-test")]
    (try
      (delete-other-windows)
      (let [buffer (status/regit-status root true)
            window (focused-window)
            decoration (diff-context-decoration buffer)]
        (test/assert decoration
          "regit status buffers should install diff context decoration")
        (is= :top (:side decoration))
        (is= 0 (:size decoration))
        (binding [*buffer* buffer
                  *window* window]
          (move-to-line buffer window (find-line (buffer-content buffer) "modified test.txt"))
          (outline/toggle-fold))
        (let [text (buffer-content buffer)
              file-line (find-line text "modified test.txt")
              hunk-line (find-line text "@@")
              content-line (find-line text "+line 1 changed")]
          (test/assert file-line "expected expanded file header line")
          (test/assert hunk-line "expected expanded hunk header line")
          (test/assert content-line "expected expanded hunk content line")
          (set-scroll-offset hunk-line window)
          (is= 1 (diff-context-decoration-size buffer))
          (set-scroll-offset file-line window)
          (is= 0 (diff-context-decoration-size buffer))
          (is= "modified test.txt"
            (diff-context-row-text (diff-context-row-at-line buffer hunk-line)))
          (is= "modified test.txt"
            (diff-context-row-text (diff-context-row-at-line buffer content-line)))))
      (finally
        (run-shell* "rm" ["-rf" root] {:direnv false})))))

(deftest regit-view-commit-diff-context-decoration-renders-hunk-file-header-test
  (let [root (repo-with-committed-change "regit-view-commit-diff-context-decoration-test")]
    (try
      (delete-other-windows)
      (let [commit-id (git-output root "rev-parse" "HEAD")
            source-buffer (create-buffer true)]
        (swap! (buffer-state source-buffer) assoc :regit-root root)
        (binding [*buffer* source-buffer]
          (let [buffer (view-commit/regit-view-commit commit-id)
                window (focused-window)]
            (assert-diff-context-renders-hunk-file-header
              buffer
              window
              "modified test.txt"
              "+line 1 changed"))))
      (finally
        (run-shell* "rm" ["-rf" root] {:direnv false})))))

(deftest regit-view-stash-diff-context-decoration-renders-hunk-file-header-test
  (let [root (repo-with-stash "regit-view-stash-diff-context-decoration-test")]
    (try
      (delete-other-windows)
      (let [source-buffer (create-buffer true)]
        (swap! (buffer-state source-buffer) assoc :regit-root root)
        (binding [*buffer* source-buffer]
          (let [buffer (view-stash/regit-view-stash "stash@{0}")
                window (focused-window)]
            (assert-diff-context-renders-hunk-file-header
              buffer
              window
              "modified test.txt"
              "+line 1 changed"))))
      (finally
        (run-shell* "rm" ["-rf" root] {:direnv false})))))
