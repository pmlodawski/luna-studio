from distutils import dir_util
import os
import subprocess
import sys

from scripts_build.atom_prepare import prep_path
from scripts_build.common import print_title, process_context, working_directory


new_gui_dir = prep_path('../luna-studio/basegl-ui')
new_gui_dist = os.path.join(new_gui_dir, 'dist')
new_gui_styles = os.path.join(new_gui_dir, 'dist/style')
atom_package = prep_path('../dist/user-config/atom/packages/luna-studio/node_modules/node_editor_basegl')
atom_styles = prep_path('../luna-studio/atom/styles/gen')


def npm_install():
    print('Installing the npm package...')
    with process_context("npm install"), working_directory(new_gui_dir):
        subprocess.call(['npm', 'install'])


def run_webpack():
    print('Running webpack...')
    with process_context("run webpack"), working_directory(new_gui_dir):
        subprocess.call(['npm', 'run', 'build'])


def copy_bundle():
    print('Copying generated JS files to Atom...')
    dir_util.copy_tree(new_gui_dist, atom_package)


def copy_styles():
    print('Copying styles to Atom...')
    dir_util.copy_tree(new_gui_styles, atom_styles)


def install():
    print_title('Installing the new GUI...')
    npm_install()
    run_webpack()
    copy_bundle()
    copy_styles()
