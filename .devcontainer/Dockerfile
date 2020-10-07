FROM alpine:3.12.0
RUN \
  apk update && \
  apk upgrade && \
  apk add bash cabal ghc git musl-dev wget && \
  cabal update
