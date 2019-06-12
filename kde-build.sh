#!bash
#
# Copyright © 2016-2019 Hartmut Goebel <h.goebel@crazy-compilers.com>
# License: GPLv3


build () {
    WHICH=$1 ; shift
    ./pre-inst-env guix package -A | grep $WHICH | \
	cut -f 1 | xargs ./pre-inst-env guix build -K "$@"
}

#build /qt.scm --fallback
build /kde-frameworks.scm --fallback
#build /kde.scm --fallback
build /kde-plasma.scm --fallback
