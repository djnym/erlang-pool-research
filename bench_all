#!/bin/sh

#      pt_baseline_sup \
for m in pt_overhead \
      pt_gsp_sup \
      pt_pooler_sup \
      pt_poolboy_sup \
      pt_gproc_sup \
      pt_dispcount_sup ; do
  echo "./bench $m | tee data/$m.dat"
  ./bench $m | tee data/$m.dat
done
