###########################################################################
## Copyright (C) Flowbox, Inc / All Rights Reserved
## Unauthorized copying of this file, via any medium is strictly prohibited
## Proprietary and confidential
## Flowbox Team <contact@flowbox.io>, 2014
###########################################################################

# OLD_PATH=$PATH # why was this set here? It discards the .zshrc settings!
echo $ZDOTDIR_ORIG/.zshrc
source $ZDOTDIR_ORIG/.zshrc
if [ -z "$FDEV_PS1" ]; then export FDEV_PS1=$PS1; fi
export PS1="[$FDEV_WORKON_HISTORY] "$FDEV_PS1
export PATH=$PATH_ORIG