#!/usr/bin/env bash
set -euo pipefail

newfname='pastes.migrate_2_3.db'
[[ -f $newfname ]] && { echo >&2 "Destination '$newfname' already exists!"; exit 1; }

echo "Copying database to '$newfname'..."
sqlite3 pastes.db "VACUUM INTO '$newfname'"

oldversion=$(sqlite3 "$newfname" 'SELECT version FROM meta')
[[ $oldversion -ne 2 ]] && { echo >&2 "Database is not currently at version 2!"; exit 1; }

echo "Migrating '$newfname'..."
sqlite3 "$newfname" <<EOF
PRAGMA foreign_keys = on;

UPDATE meta SET version = 3;

ALTER TABLE pastes ADD COLUMN srcip TEXT NULL;

VACUUM;
EOF

echo "Migrated into '$newfname'."
