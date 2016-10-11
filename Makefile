ebin = build/default/lib/cli/ebin/

compile:
	./rebar3 compile

clean:
	rm -rf build ebin rebar.lock .rebar3

test: compile
	bin/test $(TESTS)

readme:
	multimarkdown README.md > /tmp/erlang-cli-readme.html
