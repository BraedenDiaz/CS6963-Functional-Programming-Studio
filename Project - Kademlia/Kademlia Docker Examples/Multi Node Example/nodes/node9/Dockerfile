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

# Enable ipv4 forwarding
RUN echo "net.ipv4.conf.all.forwarding=1" | tee -a /etc/sysctl.conf
RUN echo "net.ipv4.conf.default.forwarding=1" | tee -a /etc/sysctl.conf

# Copy required files into the container
COPY ./files/Kademlia.rkt /kad-node
COPY ./files/racket-8.0-x86_64-linux-cs.sh /kad-node

# Install racket and SSL certificates
RUN sh /kad-node/racket-8.0-x86_64-linux-cs.sh
RUN apt-get install ca-certificates -y

# Compile Kademlia Executable
WORKDIR /kad-node
RUN ./../usr/racket/bin/raco pkg install sha
RUN ./../usr/racket/bin/raco pkg install --deps search-auto hostname
RUN ./../usr/racket/bin/raco exe Kademlia.rkt

### All nodes are the same up until this point. ###
# Thus, everything before this will be cached in order to create similar images faster.

COPY ./nodes/node9/config.txt /kad-node/

# Run Kademlia Executable
ENTRYPOINT ["/kad-node/Kademlia", "config.txt"]