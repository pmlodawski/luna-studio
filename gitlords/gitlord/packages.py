import importlib

from gitlord.io_utils import eprint


def ensure_installed(pkg_name, *, import_name=None):
    """ Install missing packages with pip.

    :param pkg_name: Package name (the name you install it with)
    :param import_name: Module name (by default the same as pkg name)
    to install.

    :type pkg_name: string
    :rtype: None
    """

    if not import_name:
        import_name = pkg_name

    try:
        importlib.import_module(import_name)

    except ImportError:

        eprint("No %s available, will install %s." % (import_name, pkg_name))
        try:
            import pip
        except ImportError as e:
            eprint("Ooops, can't install %s because... there seems to be no `pip` - the package manager." % (pkg_name, ))
            eprint("Install `pip` to continue. Try one of the following:")
            eprint("")
            eprint("    apt-get update && apt-get install python3-pip   # Ubuntu, Fedora")
            eprint("    pacman -S python-pip                            # mighty Arch")
            eprint("    # on OS X, `brew install python3` should get you `pip` as well")
            eprint("    easy_install pip                                # semi-manual, with easy_install")
            eprint("")
            eprint("If `pip` is present, then some error occured. Try re-running or get commands to call by")
            eprint("yourself by re-running with --no-pip-autoinstall flag.")

            raise Exception("No `pip` available. Strange.")

        try:
            pip.main(['install', pkg_name])

            globals()[import_name] = importlib.import_module(import_name)
        except ImportError as e:
            raise Exception("Module " + pkg_name + " unavailable") from e


    else:
        print("Imported %s successfully, assuming package %s is installed." % (import_name, pkg_name))

ensure_installed.show_warning = False
