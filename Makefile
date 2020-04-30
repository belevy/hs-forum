.PHONY: docker dev-docker

CWD=$(dir $(realpath $(firstword $(MAKEFILE_LIST))))
DOCKER_FILE=Dockerfile-backend

start: $(DOCKER_FILE)
	@docker-compose up -d

dev: $(DOCKER_FILE)
	@docker build . -f $(DOCKER_FILE) --target dev --tag hs-forum/dev:latest
	@docker-compose up -d --scale backend=0
	-docker run -it --rm -v $(CWD)backend:/var/hs-forum --network="hs-forum_internal" -p=3000:8080 --name="dev-docker" hs-forum/dev
	@docker-compose down
