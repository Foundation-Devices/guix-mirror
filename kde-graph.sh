#!bash
#
# Copyright © 2016-2019 Hartmut Goebel <h.goebel@crazy-compilers.com>
# License: GPLv3

graph () {
    WHICH="$1" ; shift
    ./pre-inst-env guix package -A | grep -E "$WHICH" | \
	head | cut -f 1 | xargs ./pre-inst-env guix graph | dot -Tpdf > kde-graph.pdf
}

#graph '/kde(|-frameworks|-plasma)\.scm'
graph '/kde-frameworks.scm'
