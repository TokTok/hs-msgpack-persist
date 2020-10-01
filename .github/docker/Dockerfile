FROM toxchat/toktok-stack:haskell AS build

RUN ["rm", "-rf", "/work/hs-msgpack-persist"]
COPY --chown=builder:users . /work/hs-msgpack-persist
RUN ["stack", "install", "msgpack-persist"]

FROM scratch
COPY --from=build /home/builder/.local/ /
