FROM bitnami/minideb:buster

ENV LANG C.UTF-8

RUN apt-get update && \
  apt-get install -y --no-install-recommends ca-certificates sqlite3


COPY bin/sensei-exe /bin

## Create special user 'user' and prepare its envornment
RUN useradd --shell /bin/bash -u 500 -o -c "" -m user
VOLUME /home/user

# Run sensei as user 'user' -> this means the server will run unprivileged
# using id 500, in the /home/user directory
USER user
WORKDIR /home/user

ENTRYPOINT ["/bin/sensei-exe" ]

STOPSIGNAL SIGTERM
