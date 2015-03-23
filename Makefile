all:
	@rebar get-deps compile

graphs: data/*.dat
	gnuplot < gnuplot_in

edoc:
	@rebar skip_deps=true doc

check:
	@rm -rf .eunit
	@mkdir -p .eunit
	@dialyzer -Wno_undefined_callbacks -Wno_opaque -Wno_return --src src
	@rebar skip_deps=true eunit

clean:
	@rebar clean

maintainer-clean:
	@rebar clean
	@rebar delete-deps
	@rm -rf deps
