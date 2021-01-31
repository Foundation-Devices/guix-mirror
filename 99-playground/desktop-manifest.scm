;; For comparism from
;; https://guix.gnu.org/de/blog/2019/running-a-guix-xfce-desktop-on-centos-7/
;;
;; (specifications->manifest
;;  '("xfce" "xfce4-session" "xfconf" "xfce4-battery-plugin"
;;    "pulseaudio" "xfce4-volumed-pulse" "xfce4-notifyd"
;;    ;; Helpful graphical programs.
;;    "mousepad" "orage"
;;    ;; System configuration utilities.
;;    "xbacklight" "pavucontrol" "stow"
;;    ;; For HTTPS access.
;;    "nss-certs"
;;    ;; These utilities are provided by the host, but we want the Guix versions
;;    ;; because they are likely better integrated and up to date.
;;    "fontconfig" "bash-completion" "gnupg" "man-db" "git"))


;; I tried several combinations of packages to get Plasma Desktop running.
;; Asking at the KDE "distribution" mailinglist did not give an reasonable
;; answer.  If you have contact to the Plasma core team, maybe someone there
;; can answer what is required to run the Plasma Desktop.
;;
;; In ../11-TODO-Plasma-Desktop-Service.txt you can find a collection of what
;; "task-plasma-desktop" or "task-plasma-desktop-minimal" meta-packages install
;; in some other distributions.
;;
;; Good Luck!


(specifications->manifest
 '("plasma-desktop"
   "plasma-workspace"  ;; propagate?
   "qtbase" ;; to not search Qt plugins in the host OS

   ;;-- theme/visual dependencies
   "breeze"  ; default theme
   "breeze-icons" ;; required: default Icon theme - propagate?
   "kirigami" "oxygen"
   "qtquickcontrols2"  ;; required by plasma-workspace and plasme-desktop
   "qqc2-desktop-style" "font-liberation"

   ;; adding qtquickcontrols2 results in:
   ;;;KCrash: Attempting to start /gnu/store/lif5pqmd3rk2cda9293gl386mp32zhi7-plasma-workspace-5.18.5/bin/.plasmashell-real
   ;;;KCrash: Application '.plasmashell-real' crashing...

   ;;-- shell/QML dependencies
   "kde-cli-tools" "kded" "kwin" "setxkbmap"
   "qtgraphicaleffects"  ;; required by plasma-workspace

   ;; --- addefd by hartmut
   ;; ksplashqml requires: qtsvg qtimageformats kimageformats
   "qtsvg" "qtimageformats" "kimageformats"

   "plasma-workspace-wallpapers" ;; dpendency of workspace already?
   "plasma-integration"  ;; - KDEPlasmaPlatformTheme.so - propagte with breeze?

   "kinit"  ;; propagate with plasma-workspace - or hardcode path?!
   "kglobalaccel"
   ;; kapplymousetheme
   "libkscreen"  ;; actually required?
   "kscreen"  ;;- branch kde-plasma - actually required?
   "kservice" ;;- Tier 3 solution for working with .desktop files. - actually required?

   ;; ---------- up to here: mini-desktop -----------------------

   ;;
   "kactivities"
   "kactivitymanagerd"  ;; - branch kde-plasma System service to manage user's activities
   "kdeplasma-addons" ;; - branch kde-plasma
   ;; "khotkeys"  ;; - branch kde-plasma  - editor komponent??
   ;; "kparts"
   "krunner"

   "kitemviews"  ;; irgendwas von da oben braucht das, ist baer nicht verbunden

   ;;"baloo" "milou"  - file indexer and search plasmoid

   ;; ;; ------------------
   ;; ;; This was an attempt to install *all* KDE framework and Plasma packages

   ;; "kcoreaddons" "ki18n" "karchive" "kconfig" "kdoctools"
   ;; "kwidgetsaddons" "kwindowsystem"
   ;; "polkit-qt"
   ;; "kcodecs" "kauth" "kcrash" "kdbusaddons"
   ;; "kguiaddons" "kconfigwidgets" "kitemviews" "kiconthemes" "kcompletion"
   ;; "kservice" "sonnet"
   ;; "attica" "breeze-icons" "kglobalaccel" "ktextwidgets" "knotifications"
   ;; "kxmlgui"
   ;; "kbookmarks" "kjobwidgets" "kwallet" "solid" "kio" "taglib" "kirigami"
   ;; "kpackage" "kparts" "kinit"
   ;; "kactivities" "kdeclarative" "kidletime" "kunitconversion"
   ;; "ksyntaxhighlighting"
   ;; ;;"plasma-wayland-protocols"
   ;; "kdnssd" "kitemmodels" "ktexteditor" "kwayland" "threadweaver"
   ;; "kded" "kdesignerplugin" "kemoticons" "kfilemetadata" "knewstuff" "kpty"
   ;; "plasma-framework"
   ;; "baloo" "kde-frameworkintegration" "kactivities-stats" "kcmutils" "kdesu"
   ;; "kholidays"
   ;; "kimageformats" "knotifyconfig" "kpeople" "kplotting" "krunner"
   ;; "kxmlrpcclient" "prison"
   ;; "purpose" "qqc2-desktop-style" "syndication" "kjs" "kdecoration"
   ;; "breeze" "libksysguard"
   ;; "kuserfeedback" "kscreenlocker"
   ;; ;;"kwayland-integration" "kwayland-server"
   ;; "libkscreen"
   ;; "khelpcenter" "kjsembed"
   ;; ;;"kquickcharts"
   ;; "kactivitymanagerd" "kwin" "milou"
   ;; ;;"kio-extras"  <--- not yet packaged
   ;; "qca"
   ;; "pulseaudio-qt" "ksysguard" "powerdevil" "systemsettings"
   ;; ;;"kaccounts-integration" " kaccounts-providers" "ksysguardqml"  ; not yet packaged
   ;; "bluedevil" "discover" "kdeplasma-addons"
   ;; "khotkeys"
   ;; ;;"kinfocenter" -- application
   ))
