#!/bin/sh

# wrapper script to pass the correct options to hsc2hs

exedir="{libexecdir}"
exeprog="hsc2hs"
executablename="$exedir/$exeprog"
topdir="{topdir}"
HSC2HS_EXTRA="--cflag=-fno-stack-protector "

tflag="--template=$topdir/include/template-hsc.h"
Iflag="-I$topdir/include/"

read_response() {
    response_file=$1
    if [ -f "$response_file" ]; then
        cat "$response_file" >> /tmp/hsc2hs_responses.txt
        while read -r arg; do
            case "$arg" in
                -t*)          tflag=;;
                --template=*) tflag=;;
                @*)           read_response "${arg#"@"}" ;;
                --)           break;;
            esac
        done < "$response_file"
    fi
}

for arg do
    case "$arg" in
        -t*)          tflag=;;
        --template=*) tflag=;;
        @*)           read_response "${arg#"@"}" ;;
        --)           break;;
    esac
done

exec "$executablename" ${tflag:+"$tflag"} $HSC2HS_EXTRA ${1+"$@"} "$Iflag"

