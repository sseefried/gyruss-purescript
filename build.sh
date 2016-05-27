VER=$(psc --version | sed 's/^.*\([0-9.]+\).*$/\1/')

if [ "$VER" != "0.8.5.0" -a "$1" != "--any" ]; then
  echo "This is only guaranteed to build with psc 0.8.5.0"
  echo "To try with another version anyway run:"
  echo "  ./build.sh --any"
  exit 1
fi


psc 'src/**/*.purs'  \
    'bower_components/*/src/**/*.purs' \
    --ffi 'bower_components/purescript-*/src/**/*.js'


