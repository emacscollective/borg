#!/bin/sh

# Copyright (C) 2016-2025 Jonas Bernoulli
#
# Author: Jonas Bernoulli <jonas@bernoul.li>
# SPDX-License-Identifier: GPL-3.0-or-later

script=$(basename $0)
USAGE="$script clone [<drone>...]
   or: $script checkout [<drone>...]"
OPTIONS_SPEC=
SUBDIRECTORY_OK=Yes
. "$(git --exec-path)/git-sh-setup"
require_work_tree
wt_prefix=$(git rev-parse --show-prefix)
cd_to_toplevel

GIT_PROTOCOL_FROM_USER=0
export GIT_PROTOCOL_FROM_USER

set -eu

usage() { die "usage: $USAGE"; }

super=$(pwd)
command=
path=

module_name () {
    git config -f .gitmodules --list |
        sed -n "s|^submodule.\([^.]*\).path=$1\$|\1|p"
}

module_hash () {
    git submodule foreach 'echo $path $sha1' |
        sed -n "s|^$1 \([^.]*\)|\1|p"
}

clone () {
    path="$1"
    modsdir="$2/modules"
    shift 2
    echo "--- [$path] ---"

    cd "$super"

    name=$(module_name "$path")
    push_remote=$(git config --includes -f .gitmodules remote.pushDefault || true)
    push_match=$(git config --includes -f .gitmodules --get-all remote.pushMatch || true)

    # Unlike is_tree_submodule_active, ignore value of submodule.active.
    if [ "$(git config --get --type=bool --default=true submodule.$name.active)" \
         = false -o -z "$(git config --get submodule.$name.url)" ]
    then
        echo "Skipping $path (not activated)"
    else
        if [ ! -e "$path"/.git ]
        then
            url=$(git config submodule.$name.url)
            args=
            rename=
            echo "Cloning $path from origin ($url)"
            case "$name,$url" in
            elpa-admin,*git.savannah.gnu.org*/git/emacs/elpa.git)
                args="--no-tags --single-branch --branch $name"
                rename=$name
                ;;
            *,*git.savannah.gnu.org*/git/emacs/elpa.git)
                args="--no-tags --single-branch --branch externals/$name"
                rename=externals/$name
                ;;
            *,*git.savannah.gnu.org*/git/emacs/nongnu.git)
                args="--no-tags --single-branch --branch elpa/$name"
                rename=elpa/$name
                ;;
            esac

            mkdir -p "$modsdir"
            git clone "$url" "$path" $args \
                --separate-git-dir "$modsdir/$name" ||
                echo "Cloning failed"

            if [ -n "$rename" ]
            then
                cd "$path"
                echo "Renaming branch $rename to main"
                git branch -m $rename main
            fi
        fi

        git config --includes -f .gitmodules --get-all submodule.$name.remote |
        while read -r remote url refspec
        do
            cd "$super"

            if [ ! -e "$path"/.git ]
            then
                echo "Cloning $path from $remote ($url)"

                mkdir -p "$modsdir"
                if git clone "$url" "$path" \
                       --separate-git-dir "$modsdir/$name"
                then
                    git remote rename origin "$remote"
                else
                    echo "Cloning failed"
                fi
            else
                cd "$path"

                if ! git remote | grep -q "^$remote\$"
                then
                    echo "Augmenting $path with remote $remote ($url)"
                    args=
                    case "$url" in
                    *git.savannah.gnu.org*/git/emacs/elpa.git)
                        args="--no-tags -t externals/$name" ;;
                    *git.savannah.gnu.org*/git/emacs/nongnu.git)
                        args="--no-tags -t elpa/$name" ;;
                    esac
                    git remote add "$remote" "$url" $args
                    git fetch "$remote" || echo "fetch failed"
                fi
            fi

            if [ -e "$super/$path/.git" ]
            then
                cd "$super/$path"

                if ! git config remote.pushDefault > /dev/null &&
                        [ "$remote" = "$push_remote" ]
                then
                    echo "Setting remote.pushDefault for $path to $remote"
                    git config remote.pushDefault "$remote"
                fi
            fi
        done

        if [ -e "$super/$path/.git" ]
        then
            cd "$super/$path"

            if [ -z "$(git config remote.pushDefault)" ]
            then
                url=$(git config remote.origin.url)
                for p in $push_match
                do
                    case $url in
                    *$p*)
                        echo "Setting remote.pushDefault for $path to origin"
                        git config remote.pushDefault origin ;;
                    esac
                done
            fi
        fi
    fi
    echo
}

checkout () {
    path="$1"
    shift
    echo "--- [$path] ---"

    cd "$super"

    name=$(module_name "$path")
    hash=$(module_hash "$path")

    if [ ! -e "$path"/.git ]
    then
        echo "Skipping $path (not populated)"
    else
        cd "$path"

        head=$(git rev-parse HEAD)
        branch=$(git symbolic-ref --short HEAD 2> /dev/null || true)
        upstream=$(git rev-parse --verify --abbrev-ref $branch@{u} 2> /dev/null || true)
        if [ "$hash" = "$head" ]
        then
            echo "ok"
        elif [ "$name" = "borg" ]
        then
            echo "Skipping $path (always using latest/checked out borg)"
            echo "    HEAD: $head"
            echo "expected: $hash"
        elif [ -n "$(git status --porcelain=v1 --ignored)" ]
        then
            echo "Skipping $path (uncommitted changes)"
            echo "    HEAD: $head"
            echo "expected: $hash"
        elif ! git diff --quiet "$head" "$upstream"
        then
            echo "Skipping $path (tip no longer matches upstream)"
            echo "    HEAD: $head"
            echo "expected: $hash"
        else
            echo "Checkout $path ($hash)"
            echo "HEAD was $(git log --no-walk --format='%h %s' HEAD)"
            if ! git reset --hard "$hash"
            then
                echo "Checkout of '$hash' into submodule path '$path' failed"
            fi
        fi
    fi
    echo
}

cmd_clone () {
    while [ $# -ne 0 ]
    do
        case "$1" in
        --) shift; break;;
        -*) usage;;
        *)  break;;
        esac
        shift
    done

    gitdir="$(realpath "$(git rev-parse --git-dir)")"

    if [ $# -ne 0 ]
    then
        for path in "$@"; do clone $path $gitdir; done
    else
        git ls-files -s | grep ^160000 | cut -f2 |
            while read path; do clone $path $gitdir; done
    fi
}

cmd_checkout () {
    while [ $# -ne 0 ]
    do
        case "$1" in
        --) shift; break;;
        -*) usage;;
        *)  break;;
        esac
        shift
    done

    if [ $# -ne 0 ]
    then
        for path in "$@"; do checkout $path; done
    else
        git ls-files -s | grep ^160000 | cut -f2 |
            while read path; do checkout $path; done
    fi
}

command=${1-}

case "$command" in
    clone|checkout) shift; "cmd_$command" "$@" ;;
    *) usage ;;
esac
