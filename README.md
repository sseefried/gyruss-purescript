This is a browser-based version of the 1979 Atari arcade classic,
implemented in [PureScript](http://purescript.org) written by Chris Waterson.

Chris Waterson's version hasn't been touched in a while and PureScript has changed a lot in the interem.  I've updated it to work on with PureScript's compile `psc` version 0.8.5.

I've specified each library version precisely in the hope that this decreases
the chances of bitrot. Builders from the future, please try to build this with `psc` version 0.8.5.

To build:

    $ bower install
    $ ./build.sh

To bundle:

    $ ./bundle.sh

This will generate the file `html/asteroids.js`