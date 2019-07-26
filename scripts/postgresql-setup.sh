#!/bin/bash

# Unoffiical bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail
IFS=$'\n\t'

progname="$0"

databasename=cexplorer
databaseuser=cexplorer

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
}

function check_for_psql {
	# Make sure we have the psql executable.
	psql -V > /dev/null 2>&1 || die "Error : Missing 'psql' executable!"
}

function check_psql_superuser {
	user=$(whoami)
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
}

# If --createdb fails because the default DB setup is not UTF-8, then the following
# fixes it (but completely destroys any existing databases).
#       $ sudo su postgres
#       $ pg_dropcluster --stop 9.X main
#       $ pg_createcluster --start -e UTF-8 9.X main

function check_psql_user {
	count=$(psql --dbname=template1 --command='\du' | sed "s/|.*//;s/ //g" | grep -c "${databaseuser}")
	if test "$count" -lt 1 ; then
		echo
		echo "Error : User '${databaseuser}' does not exist."
		echo
		echo "To create a '${databaseuser}' user:"
		echo "    $progname --create-user"
		echo
		exit 1
		fi
}

function check_connect_as_user {
	# Maybe also use this: http://www.postgresql.org/docs/9.3/static/libpq-pgservice.html
	psql --no-password --username "${databaseuser}" "${databasename}" --command='\dt' > /dev/null
	if test $? -ne 0 ; then
		echo
		echo "Error : Not able to connect as '${databaseuser}' user."
		echo
		echo "Make sure the 'pg_hba.conf' file contains:"
		echo "        local ${databaseuser}  ${databaseuser}           md5"
		echo
		echo "above any entry of the form:"
		echo "        local all       all ..."
		echo
		echo "Also make sure the password has been set up with:"
		echo
		echo "   ALTER USER \"${databaseuser}\" WITH ENCRYPTED PASSWORD 'xxxxxxxx';"
		echo
		echo "and then add the username/password to 'config/pgpass'."
		echo
		exit 1
		fi
}

function check_db_exists {
	count=$(psql -l | grep -v test_db | grep -c "${databasename}")
	if test "${count}" -ne 1 ; then
		echo
		echo "Error : No '${databasename}' database."
		echo
		echo "To create one run:"
		echo "    $progname --createdb"
		echo
		exit 1
		fi
	count=$(psql -l | grep ${databasename} | sed 's/[^|]*|[^|]*| //;s/ .*//' | grep -c UTF8)
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
}

function usage_exit {
	echo
	echo "Usage:"
	echo "    $progname --check       - Check database '${databasename}' exists and is set up correctly."
	echo "    $progname --createdb    - Create '${databasename}' database."
	echo "    $progname --recreatedb  - Drop and recreate ${databasename} database."
	echo "    $progname --dropdb      - Drop '${databasename}' database."
	echo "    $progname --dump-schema - Dump the schema of the '${databasename}' database."
	echo
	exit 0
}

function create_user {
	psql --dbname=template1 --command="CREATE USER ${databaseuser} WITH PASSWORD 'xxxxxxx';"
}

function create_db {
	createdb -T template0 --owner="${databaseuser}" --encoding=UTF8 "${databasename}"
}

function drop_db {
	dropdb --if-exists "${databasename}"
}

function create_tables {
	set -e
	psql "${databasename}" -E --quiet --no-psqlrc \
		--single-transaction -set ON_ERROR_STOP=on --username cexplorer \
		--file=schema/byron-schema.sql < /dev/null > /dev/null
}

function dump_schema {
	pg_dump -s "${databasename}"
}

# postgresql_version=$(psql -V | head -1 | sed -e "s/.* //;s/\.[0-9]*$//")

case "${1:-""}" in
	--check)
		check_for_psql
		check_psql_superuser
		check_db_exists
		check_psql_user
		check_connect_as_user
		;;
	--createdb)
		check_for_psql
		check_psql_superuser
		check_psql_user
		create_db
        create_tables
		;;
	--recreatedb)
		check_for_psql
		check_psql_superuser
		check_psql_user
		check_db_exists
		check_pgpass_file
		check_connect_as_user
		drop_db
		create_db
        create_tables
		;;
	--dropdb)
		check_for_psql
		check_psql_superuser
		drop_db
		;;
	--create-user)
		check_for_psql
		check_psql_superuser
		create_user
		;;
	--dump-schema)
		check_db_exists
		check_psql_user
		check_pgpass_file
		dump_schema
		;;
	*)
		usage_exit
		;;
	esac

echo "All good!"
exit 0
