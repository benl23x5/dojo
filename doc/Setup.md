# Setup

## Server Build

### Basic config
* Start with a fresh Debian install.
* Update hostname in `/etc/hostname`
* Add IP address to `/etc/hosts`


### Add a development user
```
$ adduser user
$ usermod -a -G sudo user
```

### Install packages
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

### Setup the database
```
$ sudo mkdir -p /srv/dojo/data/www

$ sudo sqlite3 /srv/dojo/data/dojo.db
# paste in contents of data/Create.sql to make the tables
sqlite> insert into Person values ('1', '1234', ...)  # setup admin users
sqlite> insert into User   values ('1', 'user', ...)  # setup admin users
$ sudo chown -R www-data:www-data /srv/dojo
```

### Deploy build executable into server path
```
dojo$ cabal install
dojo$ make deploy
```

### Setup apache

We want need to allow cgi in the app directory, in `sites-available/default.conf`
```
  DocumentRoot "/srv/dojo/data/www"

  <Directory "/srv/dojo/data/www">
          AllowOverride All
          Require all granted
          Options +ExecCGI
          AddHandler cgi-script .cgi
  </Directory>

$ sudo service apache2 restart
```

Add an `index.html` to automatically redirect to the cgi script.
```
dojo$ cp data/index.html /srv/dojo/test/aikikai-australia/data/www
```

### Setup HTTPS

Follow instructions on

```
https://certbot.eff.org/lets-encrypt/debianbuster-apache
```

```
$ sudo apt-get install certbot python-certbot-apache
$ sudo certbot --apache
```

The key certificate is installed in
```
/etc/letsencrypt/live/DOMAIN/
```


### Set the timezone

To get the current timezone in Debian, and list the available zones.

```
$ timedatectl
$ timedatectl list-timezones
```

Check the timezone table to see when daylight savings begins and ends

```
$ zdump -v Australia/Sydney
```

Set the effective timezone for cgi scripts in the Apache `apache2.conf`

```
 SetEnv TZ Australia/Sydney
```

```
$ sudo service apache2 restart
```
