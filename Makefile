.PHONY: dev start-services ghci start stop build clean frontend docker

CWD=$(dir $(realpath $(firstword $(MAKEFILE_LIST))))
DOCKER_FILE=Dockerfile-backend

webroot/styles:
	@mkdir -p $@
webroot/styles/style.css: webroot/styles frontend
	@cp frontend/dist/style.css $@

webroot/scripts:
	@mkdir -p $@
webroot/scripts/bundle.js: webroot/scripts frontend 
	@cp frontend/dist/bundle.js $@

frontend: 
	@$(MAKE) -s -C frontend

docker: $(DOCKER_FILE) 
	@DOCKER_BUILDKIT=1 docker build . -f $(DOCKER_FILE) --tag hs-forum/prod:latest 

hsforum-docker: docker
	@docker image save hs-forum/prod:latest -o $@

build: docker webroot/scripts/bundle.js webroot/styles/style.css

start: build 
	@docker-compose up -d

stop: 
	@docker-compose down 

frontend-dev: 
	@$(MAKE) -s -C frontend dev

start-services:
	@docker-compose up -d --scale backend=0

dev: start-services webroot/scripts/bundle.js webroot/styles/style.css
	@cd backend && \
		REDIS_HOST=127.0.0.1 \
		DB_HOST=127.0.0.1 \
		ghcid \
			--run="Web.runApp" \
			--reload="config.yaml"

ghci: 
	@cd backend && stack ghci

test: start-services
	@cd backend && stack test --fast --file-watch

clean: stop
	@docker container prune -f
	@rm -f webroot/scripts/bundle.js
	@rm -f webroot/styles/style.css
	@$(MAKE) -s -C frontend clean
