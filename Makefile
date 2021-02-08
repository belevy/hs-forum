.PHONY: dev repl start stop clean frontend docker dev-docker

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

dev-docker: $(DOCKER_FILE) backend/stack.yaml backend/package.yaml
	@DOCKER_BUILDKIT=1 docker build . -f $(DOCKER_FILE) --target dev --tag hs-forum/dev:latest 

docker: $(DOCKER_FILE) 
	@DOCKER_BUILDKIT=1 docker build . -f $(DOCKER_FILE) --tag hs-forum/prod:latest 

start: docker webroot/scripts/bundle.js webroot/styles/style.css
	@docker-compose up -d

stop: 
	@docker-compose down 

frontend-dev: 
	@$(MAKE) -s -C frontend dev

define dev-docker-up 
	@docker-compose up -d --scale backend=0 backend 
	@docker create -it --init -v $(CWD)backend:/var/hs-forum --network="hs-forum_internal" -p=8080:8080 --name="dev-docker" --net-alias="backend" hs-forum/dev $(1) > /dev/null
	@docker start dev-docker > /dev/null
	@docker-compose up -d --scale backend=0 frontend 
	-@docker attach dev-docker
	@docker rm dev-docker > /dev/null
	@docker-compose down
endef

dev: dev-docker 
	$(call dev-docker-up)

repl: dev-docker 
	$(call dev-docker-up, stack ghci --allow-different-user)

clean: stop
	@docker container prune -f
	@rm -f .dev-docker-image-built 
	@rm -f .docker-image-built
	@rm -f webroot/scripts/bundle.js
	@rm -f webroot/styles/style.css
	@$(MAKE) -s -C frontend clean
