#!/bin/bash

if [ "$#" -lt 1 ]; then
    echo "needs an argument"
    exit 1
fi



maplang() {
    # local file="$1"
    local ext=${1##*.}
    local file=$(basename "$1" ".$ext")
    lang=""

    # based on file name
    case $ext in
        # c|cpp|cc|cxx|h|hpp|hh|hxx)
        c)
            # lang=c
            dynamic_cmd="gcc -Wall -pedantic -std=c99 -o $file-dynamic $1"
            static_cmd="gcc -Wall -pedantic -std=c99 -o $file-static $1 -static"
            ;;
        f90|f95)
            # lang=fortran
            dynamic_cmd="gfortran -Wall -pedantic -o $file-dynamic $1"
            static_cmd="gfortran -Wall -pedantic -o $file-static $1 -static"
            ;;
        hs)
            # lang=haskell
            dynamic_cmd="ghc -Wall -o $file-dynamic $1 -dynamic"
            static_cmd="ghc -Wall -o $file-static $1"
            ;;
    esac

    # time and compile
    echo -e "\033[1m$dynamic_cmd\033[0m"
    time $dynamic_cmd
    echo -e "\033[1m$static_cmd\033[0m"
    time $static_cmd
    
    ls -1 --size --human-readable "$file-dynamic" "$file-static"
}

maplang $1


