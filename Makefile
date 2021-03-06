# If 'rsync' installed, use it to perform copies which only update if newer
# otherwise falling back to a plain 'cp'.
RSYNC_EXISTS := $(shell rsync --version 2>/dev/null)
ifdef RSYNC_EXISTS
CP=rsync --update --perms --executability
CP_RECURSIVE=rsync --recursive --update --perms --executability
else
CP=cp
CP_RECURSIVE=cp -rf
endif

assertInNixShell:
ifndef IN_NIX_SHELL
	$(error Must be run in a nix shell)
endif

assertInNixGhcjsShell:
ifndef NIX_GHCJS
	$(error Must be run in the nix ghcjs shell. "nix-shell -A shells.ghcjs")
endif

assertInNixGhcShell:
ifndef NIX_GHC
	$(error Must be run in the nix ghc shell. "nix-shell -A shells.ghc")
endif

cabalBuildClient: assertInNixGhcjsShell
	cabal --project-file=cabal-ghcjs.project --builddir=dist-ghcjs new-build all

nixShellBuildClient:
	nix-shell -A shells.ghcjs --run "make cabalBuildClient"

cabalBuildServer: assertInNixGhcShell
	cabal new-build all

nixShellBuildServer:
	nix-shell -A shells.ghc --run "make cabalBuildServer"

nixBuild:
	nix-build

PROD_STAGING_ROOT=staging/
DEV_STAGING_ROOT=dev-staging/
STAGING_ROOT=$(PROD_STAGING_ROOT)
prepStage:
	-mkdir $(STAGING_ROOT)
	-mkdir $(STAGING_ROOT)/Estuary.jsexe/
prepDevStage: STAGING_ROOT=$(DEV_STAGING_ROOT)
prepDevStage: prepStage

cleanStage:
	-rm -rf $(STAGING_ROOT)
cleanDevStage: STAGING_ROOT=$(DEV_STAGING_ROOT)
cleanDevStage: cleanStage

stageStaticAssets: prepStage
	$(CP_RECURSIVE) static/*.js $(STAGING_ROOT)/Estuary.jsexe/
	$(CP_RECURSIVE) static/WebDirt/ $(STAGING_ROOT)/Estuary.jsexe/WebDirt/
	$(CP_RECURSIVE) static/css-custom/ $(STAGING_ROOT)/Estuary.jsexe/css-custom/
	$(CP_RECURSIVE) static/css-source/ $(STAGING_ROOT)/Estuary.jsexe/css-source/
	$(CP_RECURSIVE) static/fonts/ $(STAGING_ROOT)/Estuary.jsexe/fonts/
	$(CP_RECURSIVE) static/icons/ $(STAGING_ROOT)/Estuary.jsexe/icons/
devStageStaticAssets: STAGING_ROOT=$(DEV_STAGING_ROOT)
devStageStaticAssets: stageStaticAssets

stageSamples: prepStage
	$(CP_RECURSIVE) static/samples/ $(STAGING_ROOT)/Estuary.jsexe/samples/
devStageSamples: STAGING_ROOT=$(DEV_STAGING_ROOT)
devStageSamples: stageSamples

GCC_PREPROCESSOR=gcc -E -x c -P -C -nostdinc
TEMPLATE_SOURCE=static/index.html.template

GET_CABAL_CLIENT_PACKAGE_NAME=python3 -c "import yaml; p = yaml.load(open('client/package.yaml', 'r')); print(p.get('name') + '-' + p.get('version', '0.0.0'), end='')"
GET_GHCJS_VERISON=ghcjs --version | sed -nre "s/.*version ([^ ]*).*/\1/p"
CABAL_CLIENT_BIN_DIR=dist-ghcjs/build/${system}/ghcjs-${GHCJS_VERSION}/${CABAL_CLIENT_PACKAGE_NAME}/x/Estuary/build/Estuary/Estuary.jsexe/
cabalStageClient: assertInNixGhcjsShell
	$(eval export CABAL_CLIENT_PACKAGE_NAME=$(shell $(GET_CABAL_CLIENT_PACKAGE_NAME)))
	$(eval export GHCJS_VERSION=$(shell $(GET_GHCJS_VERISON)))
	# compile the index.html template in development mode and stage it
	$(GCC_PREPROCESSOR) $(TEMPLATE_SOURCE) -o $(DEV_STAGING_ROOT)/Estuary.jsexe/index.html
	# stage the client js
	for part in lib out rts runmain ; do \
		$(CP) $(CABAL_CLIENT_BIN_DIR)/$$part.js $(DEV_STAGING_ROOT)/Estuary.jsexe/ ; \
		chmod a+w $(DEV_STAGING_ROOT)/Estuary.jsexe/$$part.js ; \
	done

nixShellStageClient:
	nix-shell -A shells.ghcjs --run "make cabalStageClient"

GET_CABAL_SERVER_PACKAGE_NAME=python3 -c "import yaml; p = yaml.load(open('server/package.yaml', 'r')); print(p.get('name') + '-' + p.get('version', '0.0.0'), end='')"
GET_GHC_VERISON=ghc --version | sed -nre "s/.*version ([^ ]*).*/\1/p"
CABAL_SERVER_BIN=dist-newstyle/build/${system}/ghc-${GHC_VERSION}/${CABAL_SERVER_PACKAGE_NAME}/x/EstuaryServer/build/EstuaryServer/EstuaryServer
cabalStageServer: assertInNixGhcShell
	$(eval export CABAL_SERVER_PACKAGE_NAME=$(shell $(GET_CABAL_SERVER_PACKAGE_NAME)))
	$(eval export GHC_VERSION=$(shell $(GET_GHC_VERISON)))
	# stage the server binary
	$(CP) $(CABAL_SERVER_BIN) $(DEV_STAGING_ROOT)
	chmod a+w $(DEV_STAGING_ROOT)/EstuaryServer

nixShellStageServer:
	nix-shell -A shells.ghc --run "make cabalStageServer"

nixStageClient:
	# compile the index.html template in production mode and stage it
	$(GCC_PREPROCESSOR) $(TEMPLATE_SOURCE) -DPRODUCTION -o $(STAGING_ROOT)/Estuary.jsexe/index.html
	# stage the minified client
	$(CP) result/ghcjs/estuary/bin/all.min.js $(STAGING_ROOT)/Estuary.jsexe/
	chmod a+w $(STAGING_ROOT)/Estuary.jsexe/all.min.js
	$(CP) result/ghcjs/estuary/bin/all.min.js.gz $(STAGING_ROOT)/Estuary.jsexe/
	chmod a+w $(STAGING_ROOT)/Estuary.jsexe/all.min.js.gz
nixDevStageClient: STAGING_ROOT=$(DEV_STAGING_ROOT)
nixDevStageClient: nixStageClient

nixStageServer:
	# stage the server binary
	$(CP) result/ghc/estuary-server/bin/EstuaryServer $(STAGING_ROOT)
	chmod a+w $(STAGING_ROOT)/EstuaryServer
nixDevStageServer: STAGING_ROOT=$(DEV_STAGING_ROOT)
nixDevStageServer: nixStageServer

bundleClient: cleanStage stageStaticAssets nixStageClient
	(cd $(STAGING_ROOT) && zip -r - ./Estuary.jsexe/*) > estuary-client.zip

curlReleaseClient: # this uses curl to download and unzip a recent pre-built client from a GitHub release
	rm -rf Estuary.jsexe
	curl -o temp.zip -L https://github.com/dktr0/estuary/releases/download/20190311/estuary-client-20190311.zip
	unzip temp.zip
	rm -rf temp.zip
	cp -Rf static/samples Estuary.jsexe

downloadDirtSamples:
	cd static && git clone https://github.com/TidalCycles/Dirt-Samples.git --depth 1
	find static/Dirt-Samples/* -type d -links 1 -exec $(CP_RECURSIVE) "{}" "static/samples/" \;
	rm -rf static/Dirt-Samples/

makeSampleMap:
	cd static/samples && bash ../WebDirt/makeSampleMap.sh . > sampleMap.json

clean: cleanStage cleanDevStage
	-rm -rf result/
	-rm -rf dist-newstyle/
	-rm -rf dist-ghcjs/

runDevServer: STAGING_ROOT=$(DEV_STAGING_ROOT)
runDevServer: stageStaticAssets cabalBuildServer
	cd ./$(STAGING_ROOT) && ./EstuaryServer

runServer: nixBuild stageStaticAssets stageSamples nixStageClient nixStageServer
	cd ./$(STAGING_ROOT) && ./EstuaryServer