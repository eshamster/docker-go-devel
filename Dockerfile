FROM golang:1.16.4-alpine3.13

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

RUN go install golang.org/x/tools/gopls@latest && \
    go install golang.org/x/tools/cmd/goimports@latest

COPY init.el ${emacs_home}
RUN emacs --batch --load ${emacs_home}/init.el
