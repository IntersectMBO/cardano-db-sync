#!/usr/bin/env bash

# Unoffiical bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail
IFS=$'\n\t'

progname="$0"


function die {
	echo "$1"
	exit 1
}

function check_pgpass_file {
  if test -z ${PGPASSFILE+x} ; then
	echo "Error: The PGPASSFILE env var should be set to the location of the pgpass file."
	echo
	echo "Eg for mainnet:"
	echo "export PGPASSFILE=$(pwd)/config/pgpass"
	echo
	exit 1
	fi

  if test ! -f "${PGPASSFILE}" ; then
    echo "Error: PostgreSQL password file ${PGPASSFILE} does not exist."
    exit 1
    fi

	export PGHOST=$(cut -d ":" -f 1 "${PGPASSFILE}")
	export PGPORT=$(cut -d ":" -f 2 "${PGPASSFILE}")
	export PGDATABASE=$(cut -d ":" -f 3 "${PGPASSFILE}")
	user=$(cut -d ":" -f 4 "${PGPASSFILE}")
	if [ "$user" != "*" ]; then
		export PGUSER=$user
	fi;
}

function check_for_psql {
	# Make sure we have the psql executable.
	psql -V > /dev/null 2>&1 || die "Error : Missing 'psql' executable!"
}

function check_psql_superuser {
	set +e
	psql -l "${PGDATABASE}" > /dev/null 2>&1 || psql -l > /dev/null 2>&1
	if test $? -ne 0 ; then
		echo
		echo "Error : User '${PGUSER:-$(whoami)}' can't access postgres."
		echo
		echo "To fix this, log into the postgres account and run:"
		echo "    createuser --createdb --superuser ${PGUSER:-$(whoami)}"
		echo
		exit 1
		fi
	set -e
}

function check_connect_as_user {
	psql  "${PGDATABASE}" --no-password --command='\dt' > /dev/null
	if test $? -ne 0 ; then
		echo
		echo "Error : Not able to connect as '${PGUSER:-$(whoami)}' user."
		echo
		exit 1
		fi
}

function check_db_exists {
	set +e
	count=$(psql -l "${PGDATABASE}" | grep -c "${PGDATABASE} ")
	if test "${count}" -lt 1 ; then
		echo
		echo "Error : No '${PGDATABASE}' database."
		echo
		echo "To create one run:"
		echo "    $progname --createdb"
		echo
		exit 1
		fi
	count=$(psql -l "${PGDATABASE}" | grep "${PGDATABASE} " | cut -d \| -f 3 | grep -c UTF8)
	if test "${count}" -ne 1 ; then
		echo
		echo "Error : '${PGDATABASE}' database exists, but is not UTF8."
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
	if test "$( psql "${PGDATABASE}" -tAc "SELECT 1 FROM pg_database WHERE datname='${PGDATABASE}'" )" != '1' ; then
		createdb -T template0 --owner="${PGUSER:-$(whoami)}" --encoding=UTF8 "${PGDATABASE}"
		fi
}



function drop_db {
	if test "$( psql "${PGDATABASE}" -tAc "SELECT 1 FROM pg_database WHERE datname='${PGDATABASE}'" )" = '1' ; then
		psql "${PGDATABASE}" --command="DROP OWNED BY CURRENT_USER;"
	fi
}

function list_views {
	psql "${PGDATABASE}" \
		--command="select table_name from information_schema.views where table_catalog = '${PGDATABASE}' and table_schema = 'public' ;"
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
	pg_dump -s --schema=public "${PGDATABASE}"
}

function create_snapshot {
	tgz_file=$1.tgz
	dbfile=$1.sql
	ledger_file=$2
	tmp_dir=$(mktemp --directory -t db-sync-snapshot-XXXXXXXXXX)
	echo $"Working directory: ${tmp_dir}"
	pg_dump --no-owner --schema=public "${PGDATABASE}" > "${tmp_dir}/$1.sql"
	cp "$ledger_file" "$tmp_dir/$(basename "${ledger_file}")"
	tar zcvf "${tgz_file}.tmp" --directory "${tmp_dir}" "${dbfile}" "$(basename "${ledger_file}")"
	mv "${tgz_file}.tmp" "${tgz_file}"
	rm -rf "${tmp_dir}"
	if test "$(gzip --test "${tgz_file}")" ; then
	  echo "Gzip reports the snapshot file as being corrupt."
	  echo "It is not safe to drop the database and restore using this file."
	  exit 1
	  fi
	echo "Created ${tgz_file}"
}

function restore_snapshot {
	file_count=$(find "$2" -type f -name '*.lstate' | wc -l)
	if test "${file_count}" -gt 0 ; then
	  echo "Ledger state directory ($2) is not empty. Please empty it and then retry."
	  exit 1
	  fi
	tmp_dir=$(mktemp --directory -t db-sync-snapshot-XXXXXXXXXX)
	tar -zxvf "$1" --directory "$tmp_dir"
	db_file=$(find "$tmp_dir/" -iname "*.sql")
	lstate_file=$(find "${tmp_dir}/" -iname "*.lstate")
	mv "${lstate_file}" "$2"
	psql --dbname="${PGDATABASE}" -f "${db_file}"
	rm --recursive "${tmp_dir}"
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
	echo "    - Create a db-sync state snapshot"
	echo "      $progname --create-snapshot <snapshot-file> <ledger-state-file>"
	echo
	echo "    - Restore a db-sync state snapshot."
	echo "      $progname --restore-snapshot <snapshot-file> <ledger-state-dir>"
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
		echo "The database ${PGDATABASE} has been dropped and recreated."
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
	--create-snapshot)
		check_pgpass_file
		check_db_exists
		if test $# -ne 3 ; then
		  echo "Expecting exactly 2 more arguments, the snapshot file name template and the ledger state directory."
		  exit 1
		  fi
		if test -z "$2" ; then
		  echo "Second argument should be the snapshot file name template."
		  exit 1
		  fi
		if test -z "$3" ; then
		  echo "Third argument should be the ledger state directory."
		  exit 1
		  fi
		create_snapshot "$2" "$3"
		;;
	--restore-snapshot)
		check_pgpass_file
		check_for_psql
		check_psql_superuser
		if [ ${RESTORE_RECREATE_DB:-"Y"} == "Y" ]; then
			drop_db
			create_db
		fi
		if test $# -ne 3 ; then
		  echo "Expecting exactly 2 more arguments, the snapshot file and the ledger state directory."
		  exit 1
		  fi
		if test -z "$2" ; then
		  echo "Second argument should be the snapshot file."
		  exit 1
		  fi
		if test -z "$3" ; then
		  echo "Third argument should be the ledger state directory."
		  exit 1
		  fi
		if test -f "$3" ; then
		  echo "Third argument is a file and expecting a directory."
		  exit 1
		  fi
		restore_snapshot "$2" "$3"
		;;
	*)
		usage_exit
		;;
	esac

echo "All good!"
exit 0
