def "c commands" [] {
 help commands |where is_custom == $false |select category name|group-by category|transpose|rename category sub-commands |get sub-commands |each { (build-string '(setq nu-' ($it.category|get 0) "-regexp" " (regexp-opt " "'" '("' ($it.name |str collect '" "') '")))') } |str collect (char nl) |save --raw t.el
}
