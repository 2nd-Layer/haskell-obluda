FROM opensuse/tumbleweed AS builder
RUN zypper ref && \
    zypper dup -y
RUN zypper install -y ghc \
      ghc-random-devel \
      make
WORKDIR /usr/src/
COPY ./ obluda
WORKDIR obluda
RUN make all

FROM opensuse/tumbleweed

LABEL maintainer="Mark Stopka <mark.stopka@perlur.cloud>"

ENV SERVICE_NAME "obluda"

RUN zypper ref && \
    zypper dup -y
RUN zypper install -y libnuma1 \
      shadow
COPY --from=builder /usr/src/obluda/obluda /usr/local/bin
COPY --from=builder /usr/src/obluda/dumps/ /usr/local/share/obluda/dumps/
RUN useradd -m obluda
USER obluda
