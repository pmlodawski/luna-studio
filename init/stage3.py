#!/usr/bin/env python3

# ##### Bootstrapping the repository. Stage 3. #####
# Stage 3: prepare the repository.
#

from operator import itemgetter
import shutil
from configparser import NoSectionError, NoOptionError

from io_utils import fprint
# noinspection PyUnresolvedReferences
import git
# noinspection PyUnresolvedReferences
from clint.textui import puts, colored
# noinspection PyUnresolvedReferences
from plumbum import local
# noinspection PyUnresolvedReferences
import plumbum


def bind_gitmodules():
    fprint(colored.blue("INFO: ") + "checking if _gitmodules and .gitmodules match")

    old_config = git.config.GitConfigParser('.gitmodules')
    new_config = git.config.GitConfigParser('_gitmodules')

    def test_match(conf_a, conf_b, section_a, *, show_mismatch=True, name_a, name_b):
        is_ok = True

        for a_key, a_val in sorted(conf_a.items(section_a),
                                   key=itemgetter(0)):
            try:
                b_val = conf_b.get_value(section_a, a_key)
                if show_mismatch:
                    if b_val != a_val:
                        fprint("""
                        COMPARISON: value mismatch.
                            {name_a}:{section_a}:{a_key} = {a_val}
                            {name_b}:{section_a}:{a_key} = {b_val}
                        """, colour='red')

            except NoSectionError:
                fprint("COMPARISON: {name_a}:{section_a} has no equivalent in {name_b}",
                       colour='red')
                return False

            except NoOptionError:
                fprint("MISMATCH: option {section_a}:{a_key} in {name_a} does not exist in the other",
                       colour='red')
                is_ok = False

        return is_ok

    configs_isomorphic = True

    for old_section in old_config.keys():
        test_res = test_match(old_config, new_config, old_section, name_a="OLD", name_b="NEW")
        configs_isomorphic = configs_isomorphic and test_res

    for new_section in new_config.keys():
        test_res = test_match(new_config, old_config, new_section, name_a="NEW", name_b="OLD", show_mismatch=False)
        configs_isomorphic = configs_isomorphic and test_res

    if not configs_isomorphic:
        raise Exception("Sorry, there is mismatch b/w .gitmodules and _gitmodules. Please fix that")
        # TODO: tools to help with the above?


def update_gitmodules():
    fprint(colored.blue("INFO: ") + "initialising git-modules")
    git_command = local["git"]
    git_command["submodule", "init"]()
    git_command["submodule", "sync", "--recursive"]()
    git_command["submodule", "update", "--recursive"]()


def bind_git_hooks():
    pass


def configure_repo():
    pass


def main():
    try:
        print("##############################" + colored.blue(" STAGE: 3 ") + "##############################")
        bind_gitmodules()
        update_gitmodules()
        bind_git_hooks()
        configure_repo()
    except Exception as e:
        terminal_width = shutil.get_terminal_size((80, 20)).columns
        hash_sign = "#"
        fprint("{hash_sign:#^{terminal_width}}", colour='red')
        print("Stage 3 got exception:")
        raise e

if __name__ == '__main__':
    main()
