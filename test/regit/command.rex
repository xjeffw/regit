(ns regit.tests.command
  (:require [regit.command :as regit-command]
            [regit.tests.util :refer [assert-buffer-color-at
                                      assert-buffer-face-at
                                      buffer-display-content]]
            [rex.base.buffer :as buffer]
            [rex.base.frame :as frame]
            [rex.base.keys :as keys]
            [rex.base.theme :as theme]
            [rex.string :as str]
            [rex.test :as test :refer [deftest is= is-error]]))

(defn- close-regit-command-ui! [ui-buf]
  (binding [*buffer* ui-buf]
    (when-let [cmd (regit-command/regit-command-keymap (keys/parse-key-sequence "q"))]
      (cmd))))

(deftest regit-command-choice-bracket-style-test
  (theme/load-theme :catppuccin-frappe)
  (let [buf (create-buffer)
        rendered (regit-command/choice-bracket ["one" "two" "three"] "two" "fallback" false)
        text (str/strip-properties rendered)]
    (is= "[one|two|three|fallback]" text)
    (binding [*buffer* buf]
      (set-string rendered buf))
    (assert-buffer-color-at buf text "two" :green)
    (assert-buffer-face-at buf text "one" :dimmed)
    (assert-buffer-face-at buf text "three" :dimmed)
    (assert-buffer-face-at buf text "fallback" :dimmed)))

(deftest regit-command-boolean-argument-label-test
  (theme/load-theme :catppuccin-frappe)
  (let [buf (create-buffer)
        rendered (regit-command/render-command-arg
                   {:key "- a"
                    :key-label "-a"
                    :description "Also save untracked and ignored files"
                    :argument "--all"
                    :value true})
        text (str/strip-properties rendered)]
    (is= "-a Also save untracked and ignored files (--all)" text)
    (test/assert (not (str/includes? text "--alltrue"))
      (str "boolean argument value should not be appended:\n" text))
    (binding [*buffer* buf]
      (set-string rendered buf))
    (assert-buffer-color-at buf text "--all" :green))
  (is= "-s Strategy (--strategy=ort)"
    (regit-command/render-command-arg-label
      {:key "- s"
       :key-label "-s"
       :description "Strategy"
       :argument "--strategy="
       :value "ort"})))

(deftest regit-command-multi-column-section-layout-test
  (let [start-window (focused-window)]
    (regit-command/regit-command
      {:args {}
       :actions {"a" {:label "Alpha" :fn (fn [_] nil)}
                 "b" {:label "Beta" :fn (fn [_] nil)}
                 "c" {:label "Gamma" :fn (fn [_] nil)}
                 "d" {:label "Delta" :fn (fn [_] nil)}}
       :layout [{:section "Actions"
                 :columns 3
                 :items [{:action "a"}
                         {:action "b"}
                         {:action "c"}
                         {:action "d"}]}]
       :return-window start-window})
    (let [ui-window (minibuffer-ui-window)]
      (test/assert ui-window "minibuffer-ui-window not opened")
      (let [ui-buf (window-buffer ui-window)]
        (binding [*buffer* ui-buf]
          (let [lines (str/split-lines (buffer-display-content ui-buf))]
            (is= 3 (count lines))
            (is= "Actions" (first lines))
            (test/assert (str/includes? (second lines) "a Alpha") "first row missing alpha")
            (test/assert (str/includes? (second lines) "b Beta") "first row missing beta")
            (test/assert (str/includes? (second lines) "c Gamma") "first row missing gamma")
            (test/assert (str/includes? (nth lines 2) "d Delta") "second row missing delta")))
        (close-regit-command-ui! ui-buf)))))

(deftest regit-command-horizontal-section-layout-test
  (let [start-window (focused-window)]
    (regit-command/regit-command
      {:args {}
       :actions {"l" {:label "left" :fn (fn [_] nil)}
                 "r" {:label "right" :fn (fn [_] nil)}
                 "s" {:label "second" :fn (fn [_] nil)}}
       :layout [{:horizontal-sections
                 [{:section "Left"
                   :items [{:action "l"}]}
                  {:section "Right"
                   :items [{:action "r"}
                           {:action "s"}]}]
                 :gap "     "}]
       :return-window start-window})
    (let [ui-window (minibuffer-ui-window)]
      (test/assert ui-window "minibuffer-ui-window not opened")
      (let [ui-buf (window-buffer ui-window)]
        (binding [*buffer* ui-buf]
          (let [lines (str/split-lines (buffer-display-content ui-buf))]
            (is= 3 (count lines))
            (test/assert (str/includes? (first lines) "Left") "heading row missing left section")
            (test/assert (str/includes? (first lines) "Right") "heading row missing right section")
            (test/assert (str/includes? (second lines) "l left") "first action row missing left action")
            (test/assert (str/includes? (second lines) "r right") "first action row missing right action")
            (test/assert (str/includes? (nth lines 2) "s second") "second action row missing right action")))
        (close-regit-command-ui! ui-buf)))))

(deftest regit-command-error-ui-test
  (let [start-window (focused-window)]
    (regit-command/regit-command
      {:args {}
       :actions {"x" {:label "Fail"
                      :fn (fn [_] "Push failed: test error")}}
       :layout ["Actions" {:action "x"}]
       :return-window start-window})
    (let [ui-window (minibuffer-ui-window)]
      (test/assert ui-window "minibuffer-ui-window not opened")
      (binding [*window* ui-window
                *buffer* (window-buffer ui-window)]
        (let [seq (keys/parse-key-sequence "x")
              cmd (keys/lookup-keys seq)]
          (test/assert (ifn? cmd) "could not find action command for 'x'")
          (keys/run-key-command cmd seq))))
    (let [err-window (minibuffer-ui-window)
          err-buffer (window-buffer err-window)]
      (test/assert err-window "regit-error window not opened")
      (binding [*window* err-window
                *buffer* err-buffer]
        (is= :regit-error *mode*)
        (let [text (with-read-lock [lock (buffer-text)]
                     (buffer/slice lock 0 (buffer/len-chars lock)))]
          (test/assert (str/index-of text "Push failed") "regit-error content missing")))
      (set-focused-window err-window)
      (binding [*window* err-window
                *buffer* err-buffer
                *mode* :regit-error
                *submodes* #{}]
        (frame/process-key-event (first (keys/parse-key-sequence "<esc>"))))
      (is= start-window (focused-window)))))
