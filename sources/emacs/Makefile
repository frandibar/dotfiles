all: checkdoc test

checkdoc:
	@echo Checking docs...
	emacs --batch --load checkdoc.el
	@echo Done.

test:
	emacs --batch \
		--load=ert \
		--directory=.. \
		--load=config-ledger-test.el \
		--funcall=ert-run-tests-batch-and-exit
