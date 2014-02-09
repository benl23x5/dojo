
all:
	cabal build

.PHONY: deploy
deploy: 
	cp dist/build/dojo/dojo /srv/dojo/test/aikikai-australia/data/www/dojo.cgi
	cp data/dojo-style.css  /srv/dojo/test/aikikai-australia/data/www/dojo-style.css
