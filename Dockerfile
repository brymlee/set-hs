FROM haskell:latest
RUN mkdir set-hs
WORKDIR set-hs
COPY src src
RUN ghc src/Main.hs
CMD ["/set-hs/src/Main"]
