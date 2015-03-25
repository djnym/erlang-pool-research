#!/bin/sh

lib=$1
deps=
for d in deps/* ; do
  deps="$deps -pa $d/ebin"
done
#  -boot start_sasl
exec erl \
  -noshell \
  -name pt -setcookie pt \
  $deps \
  -pa ebin \
  -pt modules "[$lib]" \
  -s pt_app \
  -eval "pt_bench:warm($lib), pt_bench:do_one($lib), init:stop()"
