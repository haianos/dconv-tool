## Enea Scioni, <enea.scioni@kuleuven.be>
## Dockerfile for dconv release

# from osrf/ros:kinetic-desktop-full-xenial 
from osrf/ros2:ardent-ros-base

# enabling sudo
RUN apt-get update && apt-get -y install sudo lua5.2 liblua5.2-dev wget git ssh unzip ros-ardent-ament-tools

# defining a user and giving him sudo privileges:
# ENV = available in dependend Dockerfiles, ARG = not available.
ARG UNAME=user
ARG UPASSWD=user
ARG UID=1000
ARG GID=1000
ENV UNAME ${UNAME} 
RUN groupadd -g $GID $UNAME &&\
    useradd -m -u $UID -g $GID -s /bin/bash $UNAME &&\
    echo "$UNAME:$UPASSWD" | chpasswd && adduser $UNAME sudo
#RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers

# switching to user and creating user directory
USER $UNAME
RUN mkdir -p "/home/${UNAME}"
RUN mkdir -p "/home/${UNAME}/.cache/luarocks"
WORKDIR "/home/$UNAME"
ENV WORKDIR "/home/$UNAME"


RUN wget https://people.mech.kuleuven.be/~u0072295/software/luarocks/lua-common-tools-0.1-2.rockspec &&\
    wget https://raw.githubusercontent.com/haianos/dconv-tool/master/dconv-tool-0.1-1.rockspec

CMD /bin/bash

# installing luarocks from sources
RUN wget http://luarocks.github.io/luarocks/releases/luarocks-2.4.4.tar.gz &&\
    tar zxvf luarocks-2.4.4.tar.gz &&\
    cd luarocks-2.4.4/ &&\
    . ${WORKDIR}/luarocks-2.4.4/configure --lua-version=5.2 &&\
    make build &&\
    echo "$UPASSWD" | sudo -S make install &&\
    cd ..

USER $UNAME

# Install common tools (Lua)
RUN echo "$UPASSWD" | sudo -S luarocks install lua-common-tools-0.1-2.rockspec
RUN echo "$UPASSWD" | sudo -S luarocks install dconv-tool-0.1-1.rockspec

# download sources for test (busted)
RUN git clone https://github.com/haianos/dconv-tool

## Install luaffi(fb), only unsed in the runtime
RUN git clone https://github.com/facebookarchive/luaffifb &&\
    cd luaffifb &&\
    echo "$UPASSWD" | sudo -S luarocks make &&\
    cd ..
  
## install runtime example with ROS2
RUN mkdir -p ${WORKDIR}/rosws/src/ &&\
    cd ${WORKDIR}/rosws/src/ &&\
    git clone https://github.com/ros2/rclc &&\
    git clone https://github.com/haianos/dconv-rclc-example &&\
    cd ..
    
RUN . /opt/ros/ardent/setup.sh &&\
    cd rosws/ &&\
    ament build --symlink-install &&\
    echo "source ${WORKDIR}/rosws/install/setup.bash" >> ~/.bashrc

