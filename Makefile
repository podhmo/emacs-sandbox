ENV ?= emacs.d
REALPATH = realpath

run:
	 emacs -Q -l ${ENV}/init.el &

clean:
	 test -d ${ENV}/3rdparty && rm -r ${ENV}/3rdparty
	 test -f ${ENV}/.downloaded && rm ${ENV}/.downloaded

install: check_env check_home
	 ln -s `${REALPATH} ${ENV}` ${HOME}/.emacs.d
