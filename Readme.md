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

```
$ uadduser user
$ usermod -a -G sudo user
$ sudo apt update
$ sudo apt install joe git apache2 ghc
```



## Debug

### On web interface: "Attempt to read only database"

The sqlite database file and the directory it is in both need to be owned by www-data.


