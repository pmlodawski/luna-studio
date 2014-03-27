source $ZDOTDIR_ORIG/.zshrc
if [ -z "$FDEV_PS1" ]; then export FDEV_PS1=$PS1; fi
export PS1="[$FDEV_WORKON_HISTORY] "$FDEV_PS1