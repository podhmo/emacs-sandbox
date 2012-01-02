ENV ?= dummy
SHELL = /bin/bash #for popd,pushd

check_home:
	(! test -d $HOME/.emacs.d) || readlink $HOME/.emacs.d

check_env:
	test -d $(ENV) && test -f $(ENV)/init.el

run: check_env
	 ! test -f $(ENV)/.downloaded && (pushd $(ENV) && . download && popd) || echo "already exist"
	 emacs -Q -l $(ENV)/init.el &

clean: check_env
	 test -d $(ENV)/3rdparty && rm -r $(ENV)/3rdparty
	 test -f $(ENV)/.downloaded && rm $(ENV)/.downloaded

install: check_env check_home
	 readlink $(HOME)/.emacs.d && rm $(HOME)/.emacs.d || echo "$(HOME)/.emacs.d is not found"
	 ln -s `realpath $(ENV)` $(HOME)/.emacs.d
