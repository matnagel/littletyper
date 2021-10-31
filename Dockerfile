FROM debian:stable
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get -yq install haskell-stack \
	&& stack upgrade --binary-only
WORKDIR repository
COPY stack.yaml package.yaml /repository/
RUN mkdir app src test && stack build --only-dependencies
COPY . /repository
ENTRYPOINT stack test
