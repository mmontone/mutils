.PHONY: docs
docs:
	sbcl --eval '(require :mutils)' --eval '(require :mutils-docs)' --eval '(mutils-docs:generate-docs)' --quit
