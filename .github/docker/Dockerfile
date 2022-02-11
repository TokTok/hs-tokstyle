FROM toxchat/toktok-stack:haskell AS build

RUN ["rm", "-rf", "/work/hs-tokstyle"]
COPY --chown=builder:users . /work/hs-tokstyle
RUN ["stack", "install", "tokstyle"]

FROM scratch
COPY --from=build /home/builder/.local/ /
