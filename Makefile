ebin = build/default/lib/cli/ebin/

compile:
	./rebar3 compile

clean:
	rm -rf build ebin rebar.lock .rebar3

test: compile
	bin/test

readme:
	multimarkdown README.md > /tmp/erlang-cli-readme.html
