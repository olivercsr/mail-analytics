FROM vlang:latest as build

#RUN adduser -D -h /build -u 9876 appuser
#USER appuser
WORKDIR /build

COPY --chown=1000 . /build

RUN v .

ENTRYPOINT ["/build/build"]

