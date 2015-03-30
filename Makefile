test:
	emacs -batch -L . -l ./tests/highlight.el -f ert-run-tests-batch-and-exit
# Local Variables:
# indent-tabs-mode: t
