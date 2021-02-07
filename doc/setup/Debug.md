
# Debug

### During cabal install
``* Missing (or bad) C library: sqlite3``

Install the sqlite3 dev library ``sudo apt install libsqlite3-dev``


### On web interface
``Attempt to read only database``

The sqlite database file and the directory it is in both need to be owned by www-data.


