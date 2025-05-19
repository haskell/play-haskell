#!/usr/bin/env bash
set -euo pipefail

newfname='pastes.migrate_1_2.db'
[[ -f $newfname ]] && { echo >&2 "Destination '$newfname' already exists!"; exit 1; }

echo "Copying database to '$newfname'..."
sqlite3 pastes.db "VACUUM INTO '$newfname'"

oldversion=$(sqlite3 "$newfname" 'SELECT version FROM meta')
[[ $oldversion -ne 1 ]] && { echo >&2 "Database is not currently at version 1!"; exit 1; }

echo "Migrating '$newfname'..."
sqlite3 "$newfname" <<EOF
PRAGMA foreign_keys = on;

DROP TABLE meta;
CREATE TABLE meta (
    version INTEGER NOT NULL
);
INSERT INTO meta (version) VALUES (2);

CREATE TABLE pastes2 (
    id INTEGER PRIMARY KEY NOT NULL,
    key BLOB NOT NULL,
    date INTEGER NULL,
    UNIQUE (key)
);
CREATE UNIQUE INDEX pastes_key ON pastes2 (key);
CREATE TABLE files (
    paste INTEGER NOT NULL,
    fname BLOB NULL,
    value BLOB NOT NULL,
    fileorder INTEGER NOT NULL,
    FOREIGN KEY (paste) REFERENCES pastes2 (id) ON DELETE CASCADE
);
CREATE INDEX files_paste ON files (paste);

INSERT INTO pastes2 (key) SELECT key FROM pastes;
INSERT INTO files (paste, value, fileorder)
    SELECT pastes2.id, pastes.value, 1 FROM pastes2, pastes WHERE pastes2.key = pastes.key;

DROP TABLE pastes;
ALTER TABLE pastes2 RENAME TO pastes;
VACUUM;
EOF

echo "Migrated into '$newfname'."
