fishGroup
===

[![Build Status](https://travis-ci.org/githubuser/fishGroup.png)](https://travis-ci.org/githubuser/fishGroup)

See https://githubuser.github.io/fishGroup/index.html for project description.

~~~
stack build --test --exec "$(stack path --local-install-root)/bin/fishGroup-example" --exec "$(stack path --local-bin)/pandoc -f markdown+lhs -i app/example.lhs -t html -o index.html --filter pandoc-include --mathjax" --file-watch
~~~
