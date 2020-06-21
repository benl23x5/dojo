# Aiki Dojo Web App

Dojo is a bare-bones web app for managing student attendence at in a martial arts or similar
organisation. Create people and events, add people to events, see what people attended
which events, etc. Uses a SqLite3 backend database.


## Setup

```
```

```
data/$
```

## Server Build

Basic config
* Update hostname in `/etc/hostname`
* Add IP address to `/etc/hosts`

Add a development user
```
$ adduser user
$ usermod -a -G sudo user
```

Install packages
```
$ sudo apt update
$ sudo apt install joe git make apache2 ghc cabal-install sqlite3 libsqlite3-dev
$ cabal update
```

This currently pulls in GHC 8.4, which is what the dojo app uses.

Clone and build dojo app
```
dojo$ git clone git@github.com:benl23x5/dojo.git
```

Setup the database
```
$ sudo mkdir -p /srv/dojo/test/aikikai-australia/data/www

$ sudo sqlite3 /srv/dojo/test/aikikai-australia/data/dojo.db
# paste in contents of data/Create.sql to make the tables
sqlite> insert into Person values ('1', '1234', ...)  # setup admin users
sqlite> insert into User   values ('1', 'user', ...)  # setup admin users
$ sudo chown -R www-data:www-data /srv/dojo
```

Deploy build executable into server path
```
dojo$ cabal install
dojo$ make deploy
```

Setup apache to allow cgi in the app directory, in `sites-available/default.conf`
```
       DocumentRoot "/srv/dojo/test/aikikai-australia/data/www"

        <Directory "/srv/dojo/test/aikikai-australia/data/www">
                AllowOverride All
                Require all granted
                Options +ExecCGI
                AddHandler cgi-script .cgi
        </Directory>

$ sudo service apache2 restart
```

Add an `index.html` to automatically redirect to the cgi script
```
dojo$ cp data/index.html /srv/dojo/test/aikikai-australia/data/www
```


## Debug

### During cabal install
``* Missing (or bad) C library: sqlite3``

Install the sqlite3 dev library ``sudo apt install libsqlite3-dev``


### On web interface
``Attempt to read only database``

The sqlite database file and the directory it is in both need to be owned by www-data.


