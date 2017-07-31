setupClient:
	stack setup --stack-yaml=client.yaml

buildClient:
	stack build --stack-yaml=client.yaml

setupServer:
	stack setup --stack-yaml=server.yaml

buildServer:
	stack build --stack-yaml=server.yaml

install:
	cp -Rf $$(stack path --local-install-root)/bin/Estuary.jsexe .
	cp -Rf static/* Estuary.jsexe

open:
	cp -Rf $$(stack path --local-install-root)/bin/Estuary.jsexe .
	cp static/index.html Estuary.jsexe
	cp static/style.css Estuary.jsexe
	open Estuary.jsexe/index.html

zip:
	rm -rf temp
	mkdir temp
	cp package.json temp
	cp estuary.js temp
	cp evalClient.js temp
	cp -Rf $$(stack path --local-install-root)/bin/Estuary.jsexe temp
	cp static/index.html temp/Estuary.jsexe
	cp static/style.css temp/Estuary.jsexe
	cp static/EstuaryProtocol.js temp/Estuary.jsexe
	tar czf estuary-build.tgz -C temp .
	rm -rf temp

zipWithWebDirt:
	rm -rf temp
	mkdir temp
	cp package.json temp
	cp estuary.js temp
	cp evalClient.js temp
	cp -Rf $$(stack path --local-install-root)/bin/Estuary.jsexe temp
	cp static/index.html temp/Estuary.jsexe
	cp static/style.css temp/Estuary.jsexe
	cp static/EstuaryProtocol.js temp/Estuary.jsexe
	cp -Rf static/WebDirt temp/Estuary.jsexe
	cd temp; zip -r ../estuary-build.zip ./*
	rm -rf temp

WebDirt:
	cp -Rf static/WebDirt Estuary.jsexe

clean:
	rm -rf Estuary.jsexe
	rm -rf $$(stack path --local-install-root)/bin/Estuary.jsexe

style:
	cp static/style.css Estuary.jsexe

static:
	cp -Rf static/* Estuary.jsexe

Estuary.jsexe:
	ghcjs -o Estuary Main.hs
	cp static/index.html Estuary.jsexe
	cp static/style.css Estuary.jsexe
