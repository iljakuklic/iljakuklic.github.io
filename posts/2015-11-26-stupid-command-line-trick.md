---
title: A stupid trick to marginally increase your command line productivity
---

Navigating directories in the command line (and elsewhere) can be annoying at times.
Number of tools exist to migitate the annoyance. For example, the `cd`{.lang-bash} command
remembers the previous path before actually changing the directory.
Then, `cd -`{.lang-bash} can be used to quickly go back to the previous location and
efficiently alternate two most recent positions `cd`{.lang-bash}-ed into.

<!--more-->

### pushd, popd

This pair of commands can be used to navigate directories as well. The `pushd`{.lang-bash}
command accepts one argument: the directory to switch to. In addition to that,
it pushes your current directory to the directory stack.
Later, `popd`{.lang-bash} can be used to switch back to the saved locations in FIFO order.

One problem with `pushd`{.lang-bash} is that you have to think about which directory to
switch to in order to save the current one to the directory stack upfront.
It would be useful to just save the current location, do some `cd`{.lang-bash}-ing to
wonder around for a bit and eventually go back using `popd`{.lang-bash}.


### The trick

``` .lang-bash
pushd .
```

Yep, that's it. (Please notice the dot being passed as an argument.)
Just save the current directory without switching to another one.

This is one of the things that took me a while to realize but seem so obvious after the fact...

\<rant\>Is it so unreasonable to expect this could be done or suggested as a hint when invoking
`pushd`{.lang-bash} without arguments? Any shells doing this?\</rant\>

### Morale of the story

Don't forget the trivial cases. They often turn out to be pretty useful.
