Assimilate Emacs packages as Git submodules
===========================================

Borg is a bare-bones package manager for Emacs packages.  It provides
only a few essential features and should be combined with other tools
such as [Magit], [`epkg`], [`use-package`], and [`auto-compile`].

Borg assimilates packages into the `~/.emacs.d` repository as Git
submodules.  An assimilated package is called a drone and a borg-based
`~/.emacs.d` repository is called a collective.

*For more information see the [announcement][init] and the [manual].*

[init]:    https://emacsair.me/2016/05/17/assimilate-emacs-packages-as-git-submodules
[repo]:    https://github.com/emacscollective/borg
[manual]:  https://emacsmirror.net/manual/borg

[`auto-compile`]: https://github.com/tarsius/auto-compile
[`epkg`]:         https://github.com/emacscollective/epkg
[`use-package`]:  https://github.com/jwiegley/use-package
[magit]:          https://github.com/magit/magit
