;; -*- lexical-binding: t; -*-

;; MacOS keys
(setq mac-command-modifier       'meta
      mac-right-command-modifier 'meta
      mac-option-modifier        nil
      mac-right-option-modifier  nil)

(require 'comp)
(setq native-comp-driver-options (cons "-mmacosx-version-min=11" native-comp-driver-options))
