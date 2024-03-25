# syntax=docker/dockerfile:1.4

ARG R_VERSION_DOCKERTAG=4.3.2
# we need slim r-ver and fat rstudio
# multistage build with copy is difficult, b/c lots of paths are different
# and you need to `docker pull` both images during building, which is expensive
ARG ROCKER_VARIANT=r-ver
# locking to a digest would improve reproducibility
# but digests are platform-specific, so to keep arm64 and amd64 in here,
# we use just a tag
FROM rocker/$ROCKER_VARIANT:$R_VERSION_DOCKERTAG AS base
ARG RSPM_SNAPSHOT_DATE
ENV RSPM_SNAPSHOT_URL=https://packagemanager.rstudio.com/cran/__linux__/jammy/$RSPM_SNAPSHOT_DATE
RUN echo "options(repos = c(CRAN = '${RSPM_SNAPSHOT_URL}'))" >> /usr/local/lib/R/etc/Rprofile.site

# metadata gets added by github docker metadata action

# R_HOME is already set by upstream rocker image,
# it stays stable across releases

# default place to mount source
ARG SOURCE_MOUNT_PATH=/root/source
RUN mkdir $SOURCE_MOUNT_PATH
WORKDIR $SOURCE_MOUNT_PATH

# setting various R library paths
# writing this outside of R_HOME makes it easier
# to deal with paths when R is changing
ARG R_LIBS_MUGGLE_ROOT=/usr/local/muggle
RUN mkdir $R_LIBS_MUGGLE_ROOT
# this is for what is absolutely necessary
ARG R_LIBS_RUNTIME=$R_LIBS_MUGGLE_ROOT/runtime
ENV R_LIBS_RUNTIME=$R_LIBS_RUNTIME
RUN mkdir $R_LIBS_RUNTIME
# this is for buildtime deps, such as pkgdown
ARG R_LIBS_BUILDTIME=$R_LIBS_MUGGLE_ROOT/buildtime
ENV R_LIBS_BUILDTIME=$R_LIBS_BUILDTIME
RUN mkdir $R_LIBS_BUILDTIME
# this is for devtime deps, such as usethis
ARG R_LIBS_DEVTIME=$R_LIBS_MUGGLE_ROOT/devtime
ENV R_LIBS_DEVTIME=$R_LIBS_DEVTIME
RUN mkdir $R_LIBS_DEVTIME

SHELL ["/bin/bash", "-c"]
RUN set -o pipefail

# system dependencies ====
ENV R_LIBS=$R_LIBS_BUILDTIME
# pak will unfortunately linger in all downstream images,
# but it won't be in the path
RUN Rscript -e "install.packages('pak')"
# workaround until https://github.com/r-lib/pak/issues/251
RUN Rscript -e "install.packages('remotes')"
# git is required for the makefile
RUN apt-get update && apt-get install --yes --no-install-recommends \
  curl \
  git
COPY .env .
COPY Makefile .
COPY DESCRIPTION .
# installing (sys) deps this early isn't great;
# it will be expensive when DESCRIPTION changes,
# and everything downstream has to be rebuilt
# however, because results of `apt-get` cannot be easily COPY --from
# this is the only way without repeating this installation
RUN make sysdeps

# R dependencies ====
# all downstream targets are supersets of this
# so it may seem superfluous
# but by making it an extra target, and `COPY --from` it later
# buildx parallelisation can be maximally leveraged
FROM base AS dep_installer
RUN make rdeps

FROM base AS builder
ENV R_LIBS=$R_LIBS_BUILDTIME:$R_LIBS_RUNTIME
ARG R_DEPS_BUILDTIME="devtools, pkgdown, rcmdcheck, roxygen2, lintr"
# when list of buildtime dep pkgs changes,
# apt-get update needs to run again,
# to ensure that apt-get install emitted by pak work
RUN apt-get update
SHELL ["Rscript", "-e"]
RUN pak::pkg_system_requirements( \
  package = strsplit(Sys.getenv("R_DEPS_BUILDTIME"), ", ")[[1]], \
  execute = TRUE, \
  sudo = FALSE, \
  echo = TRUE \
)
RUN pak::pkg_install(pkg = strsplit(Sys.getenv("R_DEPS_BUILDTIME"), ", ")[[1]])
# above can be done on base *without* package-specific installed dependencies
COPY --link --from=dep_installer $R_LIBS_RUNTIME $R_LIBS_RUNTIME

# this installs the rest to buildtime
RUN pak::local_install_deps(dependencies = TRUE)

# now come a bunch of small targets with one task each
FROM builder AS roxygeniser
# copy everything needed for roxygen,
# which also means everything loading (!)
COPY .Rbuildignore .Rbuildignore
COPY LICENSE LICENSE
COPY R ./R
COPY inst* ./inst
COPY tests* ./tests
SHELL ["/bin/sh", "-c"]
RUN make roxygenise
RUN make pkgdown

FROM base AS developer
# devtime is a superset, so no need to `COPY --from` buildtime
# but just prepend the library path
ENV R_LIBS=$R_LIBS_DEVTIME:$R_LIBS_BUILDTIME:$R_LIBS_RUNTIME
SHELL ["/bin/sh", "-c"]
RUN apt-get update && apt-get install --yes --no-install-recommends \
  python3 \
  python3-pip
RUN pip3 install --no-cache-dir \
  radian
SHELL ["Rscript", "-e"]

ARG R_DEPS_DEVTIME="usethis, languageserver, servr" 
# when list of devtime dep pkgs changes, 
# apt-get update needs to run again,
# to ensure that apt-get install emitted by pak work
RUN ["/bin/bash", "-c", "apt-get update"]
RUN pak::pkg_system_requirements( \
  package = strsplit(Sys.getenv("R_DEPS_DEVTIME"), ", ")[[1]], \
  execute = TRUE, \
  sudo = FALSE, \
  echo = TRUE \
)
RUN pak::pkg_install( \
  pkg = strsplit(Sys.getenv("R_DEPS_DEVTIME"), ", ")[[1]] \
)
COPY --link --from=builder $R_LIBS_RUNTIME $R_LIBS_RUNTIME
COPY --link --from=builder $R_LIBS_BUILDTIME $R_LIBS_BUILDTIME

FROM builder AS installer
ENV R_LIBS=$R_LIBS_RUNTIME:$R_LIBS_BUILDTIME
COPY . .
SHELL ["/bin/sh", "-c"]
RUN make install

ARG GITHUB_SHA
ENV GITHUB_SHA=$GITHUB_SHA
ARG GITHUB_REF_NAME
ENV GITHUB_REF_NAME=$GITHUB_REF_NAME
