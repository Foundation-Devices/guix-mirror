#!/usr/bin/env bash

COMMIT=$(head -1 commits.txt | sed 's! .*!!')
echo commit: $COMMIT
git cherry-pick $COMMIT

read -p "Konfikte lösen - cherry-pick contine"
git cherry-pick --continue

SCM=gnu/packages/kde-plasma.scm

NAME=$(git log -p -1 $COMMIT | grep -F '(name "' | tail -1 | sed 's/.*"\(.*\)".*/\1/')

echo name: $NAME
sed -i 's/build-system cmake-build-system/build-system qt-build-system/' $SCM
#./pre-inst-env guix refresh -u $NAME

git add gnu/packages/kde-plasma.scm
git commit --amend --no-edit '-S'

exit

NUMBER=${1?number missing}

patch=$(ls /tmp/plasma-patches/$NUMBER-*.patch)
#NAME=${patch##*-Add-}
#NAME=${NAME%*.patch}
#echo $NUMBER $NAME
git am $patch

exit

./pre-inst-env guix build refresh -u $NAME

#git gui citool --amend

exit

read -p "Kopieren "
read -p "Beschreibung "
read -p "Name in wrapper "
xdg-open https://cgit.kde.org/$NAME.git/tree/
read -p "Lizenz "

./pre-inst-env guix refresh -u $NAME

BUILDOKAY=
while [ -z "$BUILDOKAY" ] ; do
    ./pre-inst-env guix build --substitute-urls=https://ci.guix.gnu.org $NAME
    read -p "Build okay? " BUILDOKAY
done

sh ./00-test-gui-app.sh $NAME &
sleep 60s
pgrep -u $(id -u) -f store.*profile.*container.*$NAME | xargs kill
pgrep -u $(id -u) -f store.*dbus-daemon | xargs kill

git add gnu/packages/games.scm
git commit -m "gnu: Add $NAME.
