FROM debian:bookworm

# Install basic dependencies
RUN apt-get update && \
    apt-get install -y curl wget gnupg2 xz-utils git build-essential && \
    rm -rf /var/lib/apt/lists/*

# Install Guix
RUN cd /tmp && \
    wget https://git.savannah.gnu.org/cgit/guix.git/plain/etc/guix-install.sh && \
    chmod +x guix-install.sh && \
    yes | ./guix-install.sh && \
    rm guix-install.sh

# Set up Guix environment
ENV PATH="/root/.config/guix/current/bin:/var/guix/profiles/per-user/root/current-guix/bin:${PATH}"
ENV GUIX_LOCPATH="/root/.guix-profile/lib/locale"

# Guix setup and install core packages
RUN guix pull && \
    guix install shepherd guile pkg-config gcc-toolchain cmake && \
    guix install git curl

# Create workspace directory
WORKDIR /workspace

# Copy guix and shepherd configurations
COPY .config /root/.config/

# Set up shepherd user service directory
RUN mkdir -p /root/.config/shepherd

# Default command
CMD ["/bin/bash"]