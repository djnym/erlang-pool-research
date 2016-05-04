#!/bin/sh

deps=
for d in deps/* ; do
  deps="$deps -pa $d/ebin"
done
exec erl \
  -boot start_sasl \
  -name pt -setcookie pt \
  $deps \
  -pa ebin \
  -s reloader \
  -pt modules '[pt_baseline_sup,pt_gsp_sup,pt_pooler_sup,pt_poolboy_sup,pt_dispcount_sup,pt_gproc_sup]' \
  -s pt_app


