# hadolint ignore=DL3007
FROM toxchat/haskell:latest

RUN ["rm", "-rf", "/work/hs-msgpack-binary"]
COPY --chown=builder:users . /work/hs-msgpack-binary
RUN ["stack", "build", "msgpack-binary"]
