# Gyruss in PureScript

## Development

In a terminal run:

    $ pscid

Then in another terminal run:

    $ python3 -m http.server

Then load URL `http://localhost:8000/html/dev.html`


## Releasing

To build:

    $ bower install
    $ ./build.sh

To bundle:

    $ ./bundle.sh

This will generate the file `html/asteroids.js`

Then just release all the files in `html/` as a static site on your favourite host!

To test locally:

    $ cd html
    $ python3 -m http.server

Then go to URL `http://localhost:8000`