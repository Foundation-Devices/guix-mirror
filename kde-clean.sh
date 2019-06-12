#!bash
#
# Copyright © 2016-2019 Hartmut Goebel <h.goebel@crazy-compilers.com>
# License: GPLv3
#
# remove obviously old builds

./pre-inst-env guix package -A | grep /kde-framework | \
    cut -f 1,2 \
| while read name version ; do
    find /gnu/store -maxdepth 1 -name \*-$name-\* \
	-not -name \*-$version\* \
	-not -name \*.patch
done | \
    xargs guix gc -d
