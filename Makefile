.PHONY: dev repl start stop clean

CWD=$(dir $(realpath $(firstword $(MAKEFILE_LIST))))
DOCKER_FILE=Dockerfile-backend

.dev-docker-image-built: $(DOCKER_FILE) backend/
	@docker build . -f $(DOCKER_FILE) --target dev --tag hs-forum/dev:latest
	@touch $@

.docker-image-built: $(DOCKER_FILE) backend/
	@docker build . -f $(DOCKER_FILE) --tag hs-forum/prod:latest
	@touch $@

start: .docker-image-built
	@docker-compose up -d

stop: 
	@docker-compose down 

define dev-docker-up 
	@docker-compose up -d --scale backend=0
	-docker run -it --rm --init -v $(CWD)backend:/var/hs-forum --network="hs-forum_internal" -p=3000:8080 --name="dev-docker" hs-forum/dev $(1)
	@docker-compose down
endef

dev: .dev-docker-image-built
	$(call dev-docker-up)

repl: .dev-docker-image-built	
	$(call dev-docker-up, stack ghci --allow-different-user)

clean:
	@rm .dev-docker-image-built .docker-image-built
