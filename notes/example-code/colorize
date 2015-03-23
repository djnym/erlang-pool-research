#!/bin/bash
style="friendly"
/usr/local/bin/pygmentize -f rtf -O "style=$style,fontface=Courier Bold" "$1" | sed "s/\\\\f0/\\\\f0\\\\fs48/g" | pbcopy
