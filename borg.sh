#!/bin/sh

# Copyright (C) 2016-2021  Jonas Bernoulli
#
# Author: Jonas Bernoulli <jonas@bernoul.li>
# License: GPL v3 <https://www.gnu.org/licenses/gpl-3.0.txt>

hive_remote=$(git config -f .gitmodules borg.collective)
push_remote=$(git config -f .gitmodules borg.pushDefault)

toplevel=$(git rev-parse --show-toplevel)
test -n "$toplevel" || exit 2
cd "$toplevel"

git submodule--helper list |
while read -r mode sha1 stage path
do
    if test -e "$path"
    then
        name=$(git submodule--helper name "$path")

        printf "\n--- [%s] ---\n\n" "$name"

        if ! test -e "$path"/.git
        then
            git submodule--helper clone \
                --name "$name" \
                --path "$path" \
                --url "$(git config -f .gitmodules submodule."$name".url)"
        fi

        git config -f .gitmodules --get-all submodule."$name".remote |
        while read -r remote remote_url
        do
            if ! test -e "$path"/.git
            then
                git submodule--helper clone \
                    --name "$name" \
                    --path "$path" \
                    --url "$remote_url" &&
                git remote rename origin "$remote"
            else
                cd "$path"
                git remote add "$remote" "$remote_url"
                git fetch "$remote"
                cd "$toplevel"
            fi

            if test -e "$path"/.git
            then
                cd "$path"
                if test "$remote" = "$hive_remote"
                then
                    if test -e "$toplevel/.hive-maint"
                    then
                        git config remote.pushDefault "$remote"
                    else
                        branch=$(git rev-parse --abbrev-ref HEAD)
                        test -n "$branch" &&
                            git config branch.master.remote "$remote"
                    fi
                elif test "$remote" = "$push_remote"
                then
                    git config remote.pushDefault "$remote"
                fi
                cd "$toplevel"
            fi
        done

        if test -e "$path"/.git
        then
            cd "$path"
            if ! git reset --hard "$sha1"
            then
                echo >&2 "futile: Checkout of '$sha1' into submodule path '$path' failed"
                git reset --hard HEAD
                exit 1
            fi
            cd "$toplevel"
        else
            echo >&2 "futile: Clone of any remote into submodule path '$path' failed"
            exit 1
        fi
    fi
done
