FROM ubuntu

# Update repository
RUN apt-get update

# Install main base packages
RUN apt-get install apt-utils -y
RUN apt-get install iputils-ping -y
RUN apt-get install net-tools -y
RUN apt-get install systemctl -y
RUN apt-get install nano -y

# Setup folders
RUN mkdir /kad-node/
RUN mkdir /client-node/

# Enable ipv4 forwarding
RUN echo "net.ipv4.conf.all.forwarding=1" | tee -a /etc/sysctl.conf
RUN echo "net.ipv4.conf.default.forwarding=1" | tee -a /etc/sysctl.conf

# Copy required files into the container
COPY ./Kademlia.rkt /kad-node
COPY ./Client.rkt /client-node
COPY ./racket-8.0-x86_64-linux-cs.sh /kad-node

# Install racket and SSL certificates
RUN sh /kad-node/racket-8.0-x86_64-linux-cs.sh
RUN apt-get install ca-certificates -y

# Compile Kademlia Executable
WORKDIR /kad-node
RUN ./../usr/racket/bin/raco pkg install sha
RUN ./../usr/racket/bin/raco pkg install --deps search-auto hostname
RUN ./../usr/racket/bin/raco exe Kademlia.rkt

# Compile Client Executable
WORKDIR /client-node
RUN ./../usr/racket/bin/raco exe Client.rkt

COPY ./config.txt /kad-node/