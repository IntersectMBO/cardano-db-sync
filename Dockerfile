FROM ubuntu:18.04

RUN apt-get update
RUN apt-get install -y sudo bzip2 curl git xz-utils

RUN useradd -ms /bin/bash cardano && mkdir /nix /etc/nix && chown cardano /nix

USER cardano
ENV USER cardano


RUN curl https://nixos.org/nix/install | sh
ENV PATH /home/cardano/.nix-profile/bin/:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

COPY docker/nix.conf /etc/nix/nix.conf

COPY . /home/cardano/cardano-explorer

USER root
RUN chown cardano -R /home/cardano

WORKDIR /home/cardano/cardano-explorer
USER cardano


RUN nix-build docker -A dockerFileSetup -o initial-setup

USER root
RUN ./initial-setup && rm initial-setup

RUN nix-build -Q docker -A configFiles -o /etc/cardano-cfg --arg forDockerFile true

RUN ln -sv /etc/cardano-cfg/etc/runit /etc/runit && \
    ln -sv /etc/cardano-cfg/etc/service /etc/service && \
    ln -sv /usr/bin/sudo /bin/sudo && \
    rm /etc/pam.d/sudo /etc/pam.d/other && \
    ln -sv /etc/cardano-cfg/etc/pam.d/sudo /etc/pam.d/sudo && \
    ln -sv /nix/var/nix/profiles/per-user/cardano/profile/bin/deroot /bin/deroot

RUN nix-env -iA dockerFileBinaries -f docker -I nixpkgs=docker/nixpkgs --profile /nix/var/nix/profiles/per-user/cardano/profile

RUN cat /etc/sudoers | grep -v secure_path > /etc/sudoers.tmp && mv /etc/sudoers.tmp /etc/sudoers && chmod 440 /etc/sudoers

# explorer api
EXPOSE 8100
# monitoring interface
EXPOSE 80

CMD runit
