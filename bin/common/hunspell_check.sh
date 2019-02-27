#!/bin/sh

awk '{ if (match($$0,"-----BEGIN PGP SIGNATURE-----")) exit 0; else print $$0; }'  | \
    grep '^\+' | grep -v '+++' | grep -v '#use' | \
    sed 's/\+//g;s/<[^>]*>//g;s/deb.u.//g;s/dfsg.//g' | \
    hunspell -d en_US,ru_RU  -p .hunspell_default -l
