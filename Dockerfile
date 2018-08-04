FROM frolvlad/alpine-glibc:alpine-3.6

ARG work_dir=/tmp/setup
RUN mkdir ${work_dir} && \
    chmod 777 ${work_dir}

WORKDIR /root

# --- install basic libraries --- #

RUN apk add --no-cache ca-certificates wget openssh git && \
    update-ca-certificates

# --- miscs --- #

ARG dev_dir=/root/work
RUN mkdir ${dev_dir}

# --- install go --- #

ARG go_tar=go1.10.3.linux-amd64.tar.gz

RUN wget https://dl.google.com/go/${go_tar} && \
    tar zxf ${go_tar} && \
    rm ${go_tar} && \
    mv go /usr/local && \
    mkdir /root/go

ENV PATH /usr/local/go/bin:${PATH}
ENV GOPATH /root/go

# --- install emacs --- #

RUN apk add --no-cache emacs

ARG emacs_home=/root/.emacs.d
ARG site_lisp=${emacs_home}/site-lisp
ARG emacs_docs=${emacs_home}/docs

RUN mkdir ${emacs_home} && \
    mkdir ${site_lisp} && \
    mkdir ${emacs_docs}

RUN go get github.com/rogpeppe/godef && \
    go get -u github.com/nsf/gocode && \
    go get github.com/golang/lint/golint && \
    go get github.com/kisielk/errcheck

COPY init.el ${emacs_home}
RUN emacs --batch --load ${emacs_home}/init.el
