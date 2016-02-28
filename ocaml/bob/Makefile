test: test.native
	@./test.native

test.native: *.ml *.mli
	@corebuild -pkg oUnit -quiet test.native

clean:
	rm -rf _build

.PHONY: clean
