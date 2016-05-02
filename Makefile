ebin = build/default/lib/cli/ebin/

compile:
	./rebar3 compile

clean:
	rm -rf build; rm -f rebar.lock

test: compile
	bin/test

readme:
	multimarkdown README.md > /tmp/erlang-cli-readme.html
