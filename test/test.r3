REBOL [
    Title:   "Test Godot module"
    license: MIT
]

import %../godot.reb

;; Maximum verbosity
system/options/quiet: false
system/options/log/godot: 4

;; Extract the archive
extract-gpck %test.pck