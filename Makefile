.PHONY: dev repl start stop clean

CWD=$(dir $(realpath $(firstword $(MAKEFILE_LIST))))
DOCKER_FILE=Dockerfile-backend

webroot/scripts/bundle.js: frontend/*
	@cd frontend && npm run-script build

.dev-docker-image-built: $(DOCKER_FILE) backend/stack.yaml backend/package.yaml
	@docker build . -f $(DOCKER_FILE) --target dev --tag hs-forum/dev:latest
	@touch $@

.docker-image-built: $(DOCKER_FILE) backend/*
	@docker build . -f $(DOCKER_FILE) --tag hs-forum/prod:latest
	@touch $@

start: .docker-image-built webroot/scripts/bundle.js
	@docker-compose up -d

stop: 
	@docker-compose down 

elm-dev: 
	@docker-compose up -d
	-@cd frontend && npm run-script watch
	@docker-compose down

define dev-docker-up 
	@docker-compose up -d --scale backend=0 backend 
	@docker create -it --init -v $(CWD)backend:/var/hs-forum --network="hs-forum_internal" -p=8080:8080 --name="dev-docker" --net-alias="backend" hs-forum/dev $(1) > /dev/null
	@docker start dev-docker > /dev/null
	@docker-compose up -d --scale backend=0 frontend 
	-@docker attach dev-docker
	@docker rm dev-docker > /dev/null
	@docker-compose down
endef

dev: .dev-docker-image-built
	$(call dev-docker-up)

repl: .dev-docker-image-built
	$(call dev-docker-up, stack ghci --allow-different-user)

clean: stop
	@docker container prune -f
	@rm -f .dev-docker-image-built 
	@rm -f .docker-image-built
	@rm -f webroot/scripts/bundle.js
