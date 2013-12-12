#!/bin/bash

if [ "$#" -lt 1 ]; then
    echo "needs an argument"
    exit 1
fi

while getopts "ceh" option; do
    case $option in
        "c") 
            COMPILE=1
        ;;
        "e") 
            EXECUTE=1
        ;;
        "h")
            echo -e "args: \n-c: test compilation\n-e: test execution"
            exit 0
        ;;
        [?])
            echo -e "args: \n-c: test compilation\n-e: test execution"
            exit 1
        ;;
    esac
    shift
done

maplang() {
    # local file="$1"
    local ext=${1##*.}
    file=$(basename "$1" ".$ext")
    # lang=""

    # based on file name
    case $ext in
        # c|cpp|cc|cxx|h|hpp|hh|hxx)
        c)
            # lang=c
            # assume c99 standard, it's (mostly) backward compatible
            # dynamic_cmd="gcc -Wall -pedantic -std=c99 -o $file-dynamic $1"
            dynamic_cmd="gcc -o $file-dynamic $1"
            # static_cmd="gcc -Wall -pedantic -std=c99 -o $file-static $1 -static"
            static_cmd="gcc -o $file-static $1 -static"
            ;;
        f90|f95)
            # lang=fortran
            # dynamic_cmd="gfortran -Wall -pedantic -o $file-dynamic $1"
            dynamic_cmd="gfortran -o $file-dynamic $1"
            # static_cmd="gfortran -Wall -pedantic -o $file-static $1 -static"
            static_cmd="gfortran -o $file-static $1 -static"
            ;;
        hs)
            # lang=haskell
            # dynamic_cmd="ghc -Wall -o $file-dynamic $1 -dynamic"
            dynamic_cmd="ghc -o $file-dynamic $1 -dynamic"
            # static_cmd="ghc -Wall -o $file-static $1"
            static_cmd="ghc -o $file-static $1"
            ;;
    esac
}

compiletest() {
    # compile each once first so as not to get an unfair 'cold' start 
    $dynamic_cmd >> /dev/null 2>&1
    $static_cmd >> /dev/null 2>&1

    # time and compile
    echo -e "\033[1m$dynamic_cmd\033[0m"
    time $dynamic_cmd
    echo -e "\033[1m$static_cmd\033[0m"
    time $static_cmd

    # strip files, output them elsewhere
    strip "$file-static" -o "$file-static-stripped"
    strip "$file-dynamic" -o "$file-dynamic-stripped"

    echo ""
    ls -1 --size --human-readable $file-*
    echo ""
}

executetest() {
    # time executions
    echo -e "\033[1m$file-static\033[0m" 
    time ./$file-static >> /dev/null
    echo -e "\033[1m$file-static-stripped\033[0m" 
    time ./$file-static-stripped >> /dev/null
    echo -e "\033[1m$file-dynamic\033[0m" 
    time ./$file-dynamic >> /dev/null
    echo -e "\033[1m$file-dynamic-stripped\033[0m" 
    time ./$file-dynamic-stripped >> /dev/null
}
   
for arg in "$@"; do
    maplang "$arg"

    if [[ -n "$COMPILE" ]]; then
        echo "compiling! $arg"
        compiletest "$arg"
    fi

    if [[ -n "$EXECUTE" ]]; then
        echo "executing! $arg"
        executetest "$arg"
    fi
     
    exit 0
done
