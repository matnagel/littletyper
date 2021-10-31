FROM debian:stable
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get -yq install haskell-stack \
	&& stack upgrade --binary-only
ENTRYPOINT ["ls"]
