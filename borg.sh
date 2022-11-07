#!/bin/sh

# Copyright (C) 2016-2022  Jonas Bernoulli
#
# Author: Jonas Bernoulli <jonas@bernoul.li>
# SPDX-License-Identifier: GPL-3.0-or-later

push_remote=$(git config -f .gitmodules borg.pushDefault)

toplevel=$(git rev-parse --show-toplevel)
test -n "$toplevel" || exit 2
cd "$toplevel"

module_name () {
    git config -f .gitmodules --list |
        sed -n "s|^submodule.\([^.]*\).path=$1\$|\1|p"
}

git ls-files -s | grep ^160000 |
while read -r mode hash stage path
do
    if test -e "$path"
    then
        name=$(module_name "$path")

        echo "--- [$name] ---"

        if ! test -e "$path"/.git
        then
            git clone \
                "$(git config --includes -f .gitmodules submodule.$name.url)" \
                "$path" --separate-git-dir ".git/modules/$name"
        fi

        git config --includes -f .gitmodules --get-all submodule.$name.remote |
        while read -r remote remote_url
        do
            if ! test -e "$path"/.git
            then
                git clone "$remote_url" "$path" \
                    --separate-git-dir ".git/modules/$name" &&
                git remote rename origin "$remote"
            else
                cd "$path"
                if ! $(git remote | grep -q "^$remote\$" )
                then
                    git remote add "$remote" "$remote_url"
                    git fetch "$remote"
                fi
                cd "$toplevel"
            fi

            if test -e "$path"/.git
            then
                cd "$path"
                if test "$remote" = "$push_remote"
                then
                    git config remote.pushDefault "$remote"
                fi
                cd "$toplevel"
            fi
        done

        if test -e "$path"/.git
        then
            cd "$path"
            head=$(git rev-parse HEAD)
            if test "$head" != "$hash"
            then
                if test "$1" != "--reset-hard"
                then
                    echo -e "\033[0;31mSkipping checkout\033[0m (--reset-hard not specified)"
                    echo "(If you have already committed in modules since running"
                    echo "'make boostrap', then you should not use that argument.)"
                    echo "    HEAD: $head"
                    echo "expected: $hash"
                elif test -n "$(git status --porcelain=v1 --ignored)"
                then
                    echo -e "\033[0;31mSkipping checkout\033[0m (due to uncommitted changes)"
                    echo "    HEAD: $head"
                    echo "expected: $hash"
                    git status --porcelain=v1 --ignored
                else
                    prev=$(git log --no-walk --format='%h %s' HEAD)
                    if git reset --hard "$hash"
                    then
                        echo -e "\033[1mHEAD was $prev\033[0m"
                    else
                        echo -e "\033[0;31mCheckout of $hash for $name failed\033[0m"
                    fi
                fi
            fi
            cd "$toplevel"
        else
            echo >&2 "futile: Clone of any remote into submodule path '$path' failed"
            exit 1
        fi
    fi
done
