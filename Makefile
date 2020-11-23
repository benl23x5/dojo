
all:
	cabal build

.PHONY: deploy
deploy:
	cp dist/build/dojo/dojo 	/srv/dojo/test/aikikai-australia/data/www/dojo.bin
	cp local/cgi-test-aikikai.cgi  	/srv/dojo/test/aikikai-australia/data/www/dojo.cgi
	cp data/dojo-style.css  	/srv/dojo/test/aikikai-australia/data/www
	cp data/logo-aka.png  		/srv/dojo/test/aikikai-australia/data/www
	cp -r data/latex  		/srv/dojo/test/aikikai-australia/data


.PHONY: deploy-prod
deploy-prod:
	cp dist/build/dojo/dojo 	/srv/dojo/prod/aikikai-australia/data/www/dojo.bin
	cp local/cgi-prod-aikikai.cgi   /srv/dojo/prod/aikikai-australia/data/www/dojo.cgi
	cp data/dojo-style.css  	/srv/dojo/prod/aikikai-australia/data/www
	cp data/logo-aka.png 		/srv/dojo/prod/aikikai-australia/data/www
