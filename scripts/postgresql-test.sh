#!/usr/bin/env bash

# Unoffiical bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail
IFS=$'\n\t'

progname="$0"

function wait_for_postgres {
    timeout 30 sh -c \
        "until echo '\q' | psql -h $PG_DIR postgres postgres; do sleep 1; done"
}

function start_postgres {
    mkdir -p "${PG_DIR}"
    
    # Initialise database
    initdb --encoding=UTF8 --username=postgres "${DB_DIR}"
    postgres -D "${DB_DIR}" -c listen_addresses="" -k "${PG_DIR}" &

    # Verify Postgres is running
    if wait_for_postgres; then
        echo "PostgreSQL server is verified to be started."
    else
        echo "Failed to connect to local PostgreSQL server."
        exit 2
    fi

    psql -h "${PG_DIR}" postgres postgres <<EOF
        create role $DB_USER with createdb login password NULL;
        alter user $DB_USER with superuser;
        create database $DB_NAME with owner = $DB_USER;
        \\connect $DB_NAME
        ALTER SCHEMA public OWNER TO $DB_USER;
EOF
}

function stop_postgres {
    head -n 1 "${DB_DIR}/postmaster.pid" | xargs kill
}


function usage {
    echo
    echo "Usage:"
    echo "    $progname [options] start    Initializes and starts a test database server"
    echo "    $progname [options] stop     Stops the test database server"
    echo
    echo "  Creates a test database server"
    echo
    echo "Options:"
    echo '  -d directory    Postgres data directory [default: db]'
    echo '  -n db-name      Postgres database name [default: user]'
    echo '  -s socket-dir   Directory of the unix-socket in [default: .]'
    echo '  -u user         Postgres database user [default: current user]'
 }

set -e

while getopts "d:n:s:u:h" arg; do
    case "${arg}" in
        d)
            DB_DIR="${OPTARG}"
            ;;
        n)
            DB_NAME="${OPTARG}"
            ;;
        s)
            PG_DIR="${OPTARG}"
            ;;
        u)
            DB_USER="${OPTARG}"
            ;;
        h)
            echo "h"
            usage
            exit 0
            ;;
        *)
            usage
            exit 1
            ;;
    esac
done

if [[ ! ${DB_DIR+DEFINED} ]]; then
    DB_DIR="$(pwd)/db"
fi

if [[ ! ${DB_USER+DEFINED} ]]; then
    DB_USER="$(whoami)"
fi

if [[ ! ${DB_NAME+DEFINED} ]]; then
    DB_NAME="$DB_USER"
fi

if [[ ! ${PG_DIR+DEFINED} ]]; then
    PG_DIR="$(pwd)"
fi

DB_DIR=$(realpath "$DB_DIR")
PG_DIR=$(realpath "$PG_DIR")

COMMAND="${*:$OPTIND:1}"

case $COMMAND in
    "start")
        start_postgres
        ;;
    "stop")
        stop_postgres
        ;;
    *)
        echo "A command must be specified!"
        usage
        exit 3
        ;;
esac
