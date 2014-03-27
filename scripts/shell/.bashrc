###########################################################################
## Copyright (C) Flowbox, Inc / All Rights Reserved
## Unauthorized copying of this file, via any medium is strictly prohibited
## Proprietary and confidential
## Flowbox Team <contact@flowbox.io>, 2014
###########################################################################

source ~/.bashrc
if [ -z "$FDEV_PS1" ]; then export FDEV_PS1=$PS1; fi
export PS1="[$FDEV_WORKON_HISTORY] "$FDEV_PS1