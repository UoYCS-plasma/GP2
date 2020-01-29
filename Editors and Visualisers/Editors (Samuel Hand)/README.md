In order to run the editors, only the content of the docs folder is needed. Open ``index.html`` in your web browser of choice to use the graph editor. For the rule editor, use the ``index.html`` file in the ``/rule`` subdirectory. 

If you wish to modify the editors, you can build the project using Elm. Start by running the following command in the folder containing this readme.
```
elm init
```

The editors need several packages which can be installed via elm:
```
elm install elm-community/basics-extra; elm install elm/svg; elm install elm-community/intdict; elm install pzp1997/assoc-list; elm install elm-community/graph; elm install elm-community/list-extra; elm install elm/file; elm install elm/parser; elm install elm/json; elm install brandly/elm-dot-lang
```

To build simply run the following command.
```
elm make src/Main.elm --output docs/main.js; elm make src/Rule.elm --output docs/rule.js
```
The compilation only results in the ``main.js`` and ``rule.js`` files.

All the code was developed by Samuel Hand, except dist/full.render.js and dist/viz.js, which are both part of the viz.js JavaScript library.
