.PHONY: rel deps

all: deps compile

compile:
	./rebar compile

deps:
	./rebar get-deps

clean:
	./rebar clean

##
## Release targets
##
rel:
	rm -rf rel/onecached
	./rebar compile generate

relclean:
	rm -rf rel/onecached

install: rel

test: 
	./rebar eunit