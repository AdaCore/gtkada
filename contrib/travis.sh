#!/bin/bash
set -e -x
export PATH=$HOME/gnat/bin:$PATH

function linux_before_install()
{
  sudo apt-get update
  sudo apt-get install -y \
    libx11-xcb1 \
    fontconfig \
    dbus \
    wget \
    make \
    libc-dev \
    git \
    libgtk-3-dev \

  git clone https://github.com/AdaCore/gnat_community_install_script.git
  wget -nv -O ./gnat-install \
        http://mirrors.cdn.adacore.com/art/5b0d7bffa3f5d709751e3e04
  sh gnat_community_install_script/install_package.sh \
      ./gnat-install $HOME/gnat
  gprinstall --uninstall gnatcoll
  rm -rf gnat-install
}

function linux_script()
{
./configure --disable-static --disable-static-pic --prefix=$HOME/adalib --with-GL=no
  make all install
  mkdir upload
  tar czf upload/gtkada-$TRAVIS_BRANCH-$TRAVIS_OS_NAME.tar.gz -C $HOME/adalib .
}

${TRAVIS_OS_NAME}_$1
