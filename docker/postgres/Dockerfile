FROM postgres:17

# Build dependencies for pg_semver
RUN apt-get update && apt-get install -y \
    build-essential \
    postgresql-server-dev-17 \
    git \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Install pgjwt (pure SQL extension)
RUN git clone https://github.com/michelp/pgjwt.git /tmp/pgjwt \
    && cd /tmp/pgjwt \
    && make install \
    && rm -rf /tmp/pgjwt

# Install pg_semver
RUN git clone https://github.com/theory/pg-semver.git /tmp/pg-semver \
    && cd /tmp/pg-semver \
    && make \
    && make install \
    && rm -rf /tmp/pg-semver

# Clean up build deps
RUN apt-get purge -y build-essential postgresql-server-dev-17 git \
    && apt-get autoremove -y \
    && rm -rf /var/lib/apt/lists/*
