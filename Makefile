ENV ?= emacs.d
SHELL = /bin/bash #for popd,pushd
REALPATH = grealpath
READLINK = greadlink

check_home:
	(! test -d ~/.emacs.d) || ${READLINK} ~/.emacs.d

check_env:
	test -d ${ENV} && test -f ${ENV}/init.el

run: check_env
	 ! test -f ${ENV}/.downloaded && (pushd ${ENV} && . download && popd) || echo "already exist"
	 emacs -Q -l ${ENV}/init.el &

clean: check_env
	 test -d ${ENV}/3rdparty && rm -r ${ENV}/3rdparty
	 test -f ${ENV}/.downloaded && rm ${ENV}/.downloaded

install: check_env check_home
	 ln -s `${REALPATH} ${ENV}` ${HOME}/.emacs.d
