#!bash
#
# Copyright © 2016-2019 Hartmut Goebel <h.goebel@crazy-compilers.com>
# License: GPLv3


refresh () {
    WHICH="$1" ; shift
    ./pre-inst-env guix package -A | grep -E "$WHICH" | \
	cut -f 1 | xargs ./pre-inst-env guix refresh --update
}


refresh_to_version () {
    WHICH="$1" ; shift
    VERSION="$1" ; shift
    packages=$(./pre-inst-env guix package -A | grep -E "$WHICH" | \
		      cut -f 1)
    url=https://download.kde.org/stable/plasma/$VERSION
    for pkg in $packages ; do
	hash=$(guix download $url/$pkg-$VERSION.tar.xz 2>/dev/null | tail -1)
	echo $pkg $hash
    done
}


download_src () {
    WHICH="$1" ; shift
    ./pre-inst-env guix package -A | grep -E "$WHICH" |\
	cut -f 1 | xargs ./pre-inst-env guix build --source -K
}

#refresh '/kde(|-frameworks|-plasma).scm'
#download_src '/kde(|-frameworks|-plasma)\.scm'
#refresh '/kde-frameworks.scm'
#refresh '/kde-plasma.scm'
refresh_to_version '/kde-plasma.scm' 5.13.5
