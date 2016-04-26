ebin = build/default/lib/cli/ebin/

compile:
	./rebar3 compile

clean:
	./rebar3 clean

test: compile
	bin/test

readme:
	multimarkdown README.md > /tmp/erlang-cli-readme.html
