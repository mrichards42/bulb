# Bulb

A "core" library for [Fennel][fennel]. Very much a work-in-progress.

## Inspirations and comparisons

### [Cljlib][cljlib]

Cljlib reproduces much much of clojure.core than Bulb does. It includes a
number of features, data structures, and macros that Bulb does not, e.g.
multi-arity functions, sets, multimethods, when-let (and relatives).

Bulb is not nearly as fully-featured as Cljlib, though it is also inspired by
Clojure. A main difference between the two libraries is that Cljlib's sequences
are primarily based around tables (and are more or less eager), while Bulb's
are implemented using Lua iterators, making them lazy.

### [Luafun][luafun]

Bulb and Luafun have a lot of overlap in terms of both functionality and -- in
my own benchmarking -- speed (similar on Luajit, and Bulb seems to be a bit
faster on Lua 5.4, perhaps since luafun is optimized for Luajit).

Luafun uses a slightly different iteration protocol, storing state in the
[control variable][control-var] that you are supposed to ignore. This allows
Luafun to return state*less* iterators (as long as the source iterator is
stateless), which have some [benefits][luafun-under-the-hood].

Bulb works exclusively with state*ful* iterators, and returns iterators that
work with Lua's normal iteration protocol (i.e. you can use then with the
generic `for`), though you have to wrap Lua's stateless iterators (e.g. `pairs`
and `ipairs`) in order to use them.

## License

Copyright © 2021 Mike Richards

Released under the MIT license.


[fennel]: https://fennel-lang.org
[cljlib]: https://gitlab.com/andreyorst/fennel-cljlib/
[luafun]: https://github.com/luafun/luafun
[control-var]: https://www.lua.org/pil/7.2.html
[luafun-under-the-hood]: https://luafun.github.io/under_the_hood.html#iterator-types
