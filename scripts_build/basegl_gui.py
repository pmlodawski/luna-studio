from distutils import dir_util
import os

from scripts_build.atom_prepare import prep_path
from scripts_build.common import print_title


new_gui_dir = prep_path('../luna-studio/basegl-ui')
new_gui_styles = os.path.join(new_gui_dir, 'dist/style')
atom_styles = prep_path('../luna-studio/atom/styles/gen')


def copy_styles():
    print('Copying styles to Atom...')
    dir_util.copy_tree(new_gui_styles, atom_styles)


def install():
    print_title('Installing the new GUI...')
    copy_styles()
