test:
	emacs -batch -L . -l ./tests/highlight.el -l ./tests/indentation.el -f ert-run-tests-batch-and-exit
# Local Variables:
# indent-tabs-mode: t
