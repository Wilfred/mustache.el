test:
	emacs --version
	cask install
	cask exec emacs -Q -batch \
		-L . -l mustache-tests \
		-f ert-run-tests-batch-and-exit
