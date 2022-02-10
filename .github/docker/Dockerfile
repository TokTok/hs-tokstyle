# hadolint ignore=DL3007
FROM toxchat/haskell:latest

RUN ["rm", "-rf", "/work/hs-tokstyle"]
COPY --chown=builder:users . /work/hs-tokstyle
RUN ["stack", "build", "tokstyle"]
