docker run --rm -i -t -v "$(pwd)":/src/ -w /src/ nakkaya/ferret-build /bin/bash -c './build && cd src/ && lein test'
