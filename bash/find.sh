#!/bin/bash

pars_by()
{
    if [[ ${2##*/} == $1 ]]
    then
        echo "$2"
    fi
}


check_type()
{
    case $1 in
        "d" )
            if [ -d "$2" ] ;
            then
                pars_by "$3" "$2"
            fi
            ;;
        "b" )
            if [ -b "$2" ] ;
            then 
                pars_by "$3" "$2"
            fi
            ;;
        "c" )
            if [ -c "$2" ] ;
            then
                pars_by "$3" "$2"
            fi
            ;;
        "p" )
            if [ -p "$2" ] ;
            then
                pars_by "$3" "$2"
            fi
            ;;
        "f" )
            if [ -f "$2" ] ;
            then
                pars_by "$3" "$2"
            fi
            ;;
        "l" )
            if [ -L "$2" ] ;
            then
                pars_by "$3" "$2"
            fi
            ;;
        "s" )
            if [ -S "$2" ] ;
            then
                pars_by "$3" "$2"
            fi
            ;;
        * )
            if [[ -e "$2" ]] #|| [[ -d "$2" ]] ;
            then
#                echo win "$2"
                pars_by "$3" "$2"
            fi
            ;;
    esac

    
}

find_tree()
{
    for e in "$1"/\.*
    do
        if [[ "$e" != "$1/." && "$e" != "$1/.." ]]
        then
            check_type "$2" "$e" "$3"
            if [[ -d $e ]]
            then
#                echo "directory" $e
                find_tree "$e" "$2" "$3"
            fi
        fi
    done
    for e in "$1"/*
    do
        check_type "$2" "$e" "$3"
        if [[ -d $e ]]
        then
#            echo "directory" $e
            find_tree "$e" "$2" "$3"
        fi
    done 
}

pat="*"
d="."
t=NULL

if (("$#" > 0)) ;
then
    d=$1
fi

for ((i = 2; i < $#; ++i)) ;
do
    case ${!i} in
        "-iname" )
            let j=$i+1
            pat=${!j}
#            echo "$j  $i iname=$pat"
            ;;
        "-type" )
            let j=$i+1
            t=${!j}
            ;;
        *)
            ;;
    esac
done

check_type "$t" "$d" "$pat"

if [ -e $d ]; 
then
    find_tree "$d" "$t" "$pat"
else
    echo "No such file or directory"
fi 

exit 0

