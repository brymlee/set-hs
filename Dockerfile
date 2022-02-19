FROM haskell:latest
RUN mkdir set-hs
WORKDIR set-hs
COPY src src
RUN ghc src/Data/Set.hs src/Main.hs
CMD ["/set-hs/src/Main"]
