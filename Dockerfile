FROM golang:1.12.7-alpine3.10

WORKDIR /root

# --- install basic libraries --- #

RUN apk add --no-cache ca-certificates wget openssh git && \
    update-ca-certificates

# --- miscs --- #

ARG dev_dir=/root/work
RUN mkdir ${dev_dir}

# --- install emacs --- #

RUN apk add --no-cache emacs

ARG emacs_home=/root/.emacs.d
ARG site_lisp=${emacs_home}/site-lisp
ARG emacs_docs=${emacs_home}/docs

RUN mkdir ${emacs_home} && \
    mkdir ${site_lisp} && \
    mkdir ${emacs_docs}

RUN go get -u golang.org/x/tools/cmd/gopls && \
    go get golang.org/x/tools/cmd/goimports

COPY init.el ${emacs_home}
RUN emacs --batch --load ${emacs_home}/init.el
