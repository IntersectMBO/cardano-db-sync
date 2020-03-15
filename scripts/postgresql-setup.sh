#!/usr/bin/env bash

# Unoffiical bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail
IFS=$'\n\t'

progname="$0"

PGPASSFILE=config/pgpass

function die {
	echo "$1"
	exit 1
}

function check_pgpass_file {
  if test ! -f "${PGPASSFILE}" ; then
    echo "Error: PostgeSQL password file ${PGPASSFILE} does not exist."
    exit 1
    fi

	export databasename=$(sed --regexp-extended 's/[^:]*:[^:]*://;s/:.*//' ${PGPASSFILE})
}

function check_for_psql {
	# Make sure we have the psql executable.
	psql -V > /dev/null 2>&1 || die "Error : Missing 'psql' executable!"
}

function check_psql_superuser {
	user=$(whoami)
	set +e
	psql -l > /dev/null 2>&1
	if test $? -ne 0 ; then
		echo
		echo "Error : User '$user' can't access postgres."
		echo
		echo "To fix this, log into the postgres account and run:"
		echo "    createuser --createdb --superuser $user"
		echo
		exit 1
		fi
	set -e
}

function check_connect_as_user {
	psql  "${databasename}" --no-password --command='\dt' > /dev/null
	if test $? -ne 0 ; then
		echo
		echo "Error : Not able to connect as '$(whoami)' user."
		echo
		exit 1
		fi
}

function check_db_exists {
	set +e
	count=$(psql -l | grep -v "${databasename}-tests" | grep -c "${databasename}")
	if test "${count}" -lt 1 ; then
		echo
		echo "Error : No '${databasename}' database."
		echo
		echo "To create one run:"
		echo "    $progname --createdb"
		echo
		exit 1
		fi
	count=$(psql -l | grep -v "${databasename}-tests" | grep ${databasename} | sed 's/[^|]*|[^|]*| //;s/ .*//' | grep -c UTF8)
	if test "${count}" -ne 1 ; then
		echo
		echo "Error : '${databasename}' database exists, but is not UTF8."
		echo
		echo "To fix this you should drop the current one and create a new one using:"
		echo "    $progname --dropdb"
		echo "    $progname --createdb"
		echo
		exit 1
		fi
	set -e
}

function create_db {
	createdb -T template0 --owner=$(whoami) --encoding=UTF8 "${databasename}"
}

function drop_db {
	dropdb --if-exists "${databasename}"
}

function list_views {
	psql "${databasename}" \
		--command="select table_name from information_schema.views where table_catalog = 'cexplorer' and table_schema = 'public' ;"
}

function create_migration {
	echo "To create a migration:"
	echo "cabal run create-migration --mdir schema/"
	exit 0
}

function run_migrations {
	echo "To run migrations:"
	echo "cabal run cardano-db-tool run-migrations --mdir schema/ --ldir ."
	echo "You probably do not need to do this."
	exit 0
}

function dump_schema {
	pg_dump -s "${databasename}"
}

function usage_exit {
	echo
	echo "Usage:"
	echo "    $progname --check             - Check database exists and is set up correctly."
	echo "    $progname --createdb          - Create database."
	echo "    $progname --dropdb            - Drop database."
	echo "    $progname --list-views        - List the currently definied views."
	echo "    $progname --recreatedb        - Drop and recreate database."
	echo "    $progname --create-user       - Create database user (from config/pgass file)."
	echo "    $progname --create-migration  - Create a migration (if one is needed)."
	echo "    $progname --run-migrations    - Run all migrations applying as needed."
	echo "    $progname --dump-schema       - Dump the schema of the database."
	echo
	exit 0
}

# postgresql_version=$(psql -V | head -1 | sed -e "s/.* //;s/\.[0-9]*$//")

set -e

case "${1:-""}" in
	--check)
		check_pgpass_file
		check_for_psql
		check_psql_superuser
		check_db_exists
		check_connect_as_user
		;;
	--createdb)
		check_pgpass_file
		check_for_psql
		check_psql_superuser
		create_db
		;;
	--dropdb)
		check_pgpass_file
		check_for_psql
		check_psql_superuser
		drop_db
		;;
	--list-views)
		check_pgpass_file
		check_for_psql
		check_psql_superuser
		check_db_exists
		check_connect_as_user
		list_views
		;;
	--recreatedb)
		check_pgpass_file
		check_for_psql
		check_psql_superuser
		check_db_exists
		check_connect_as_user
		drop_db
		create_db
		echo "The database ${databasename} has been dropped and recreated."
		echo "The tables will be recreated when the application is run."
		exit 0
		;;
	--create-user)
		check_pgpass_file
		check_for_psql
		check_psql_superuser
		create_user
		;;
	--create-migration)
		create_migration
		;;
	--run-migrations)
		run_migrations
		;;
	--dump-schema)
		check_pgpass_file
		check_db_exists
		dump_schema
		;;
	*)
		usage_exit
		;;
	esac

echo "All good!"
exit 0
