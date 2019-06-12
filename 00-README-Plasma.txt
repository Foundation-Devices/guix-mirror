This branch contains my work for bringing the KDE Plasma dekstop to Guix.

Hartmut Goebel, Juni 2019 & January 2021

Note 1: It might be worth making an older version of Plasma work, as this an
        older version has less dependencies. This is why this branch still
        sticks at 5.19.5.

Note 2: https://invent.kde.org/plasma lists about 60 packages, many of which
     	are applications.  I expect most of these
     	applications to be not required to start the Plasma Desktop.
	See 10-TODO-plasma.txt for more information.


About the other Readme-files
===============================

* 10-TODO-….txt contain the status of my efforts (which might be outdated) and
  a lot of snippets useful for building packages and making tests pass.

  Please at least skim through these files as they contain lots hof hints and
  some gems :-)

  The most central of this is the "Status Unit-tests" in
  10-TODO-plasma.txt. This documents the status of building the packages
  required for plasma. I'm encouraging you to use this list to avoid getting
  lost :-)


Proposed approach
==========================

  1. Make the packages listed in "Status Unit-tests" in
     10-TODO-plasma.txt. build and (most) tests pass. This list hopefully
     contains all package required for a minimal Plasma desktop service.

     I suggest using the plasma versions currently package on this branch to
     avoid introducing more issues.

  2. Build a system (see gnu/system/examples/plasma.tmpl and
     gnu/services/desktop.scm) which starts runs a small (or even minimal)
     Plasma desktop.

  3. Define two (maybe more) desktop-services:
     - plasma-minimal
     - plasma
     (- plasma-all-bells-and-wistels)


Notes about my Commit messages
====================================

  I'm using pre- and postfixes in my commit messages:

  - TEMP: This is an intermediate commit to preserve some state. This might be
    dropped, merged, fixed, etc. later.
  - REWORD: Message needs rewording (e.g. is not written well or unfinished)
  - FIXUP: This is a fixup for another commit, which should be `fixup`d using
    `git rebase -i` soon.
  - SQUASH: same, but to be `sqash`ed.
  - WIP: work in progress, commited to preserve acceptable state
  - TODO: This is unfinished work


Further reading
========================

* https://lists.gnu.org/archive/html/guix-devel/2017-11/msg00161.html
* https://lists.gnu.org/archive/html/guix-devel/2017-10/msg00185.html
