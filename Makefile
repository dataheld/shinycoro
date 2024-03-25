#!make

# sometimes git_ref_name may be "" empty string
# such as during codespace prebuilds
git_ref_name ?= $(shell git rev-parse --abbrev-ref HEAD || echo latest)
# above git ref may not be valid docker tag
tag_from_git_ref_name := $(shell echo ${git_ref_name} | sed 's/[^a-zA-Z0-9]/-/g')
tag_from_git_sha ?= latest
can_push := false
# this can be conveniently overwritten for --print and metadata file
bake_args ?= --progress auto
bake_targets := builder developer rstudio runner
smoke_test_jobs := $(addprefix smoke-test-,${bake_targets})

include .env

all: all-outside-docker all-in-docker

all-outside-docker: superlint bake 
	make bake
	make smoke-tests
	make health-checks
	make superlint

all-in-docker: rlint test check pkgdown test-shinytest

bake:
	TAG_FROM_GIT_SHA=$(tag_from_git_sha) \
		TAG_FROM_GIT_REF_NAME=$(tag_from_git_ref_name) \
		CAN_PUSH=$(can_push) \
		docker buildx bake \
			--file docker-bake.hcl \
			--file .env \
			$(bake_args)

# when the list of buildtime dep pkgs changes, 
# apt-get update needs to run again,
# to ensure that apt-get install emitted by pak work
sysdeps: DESCRIPTION
	apt-get update
	Rscript -e "pak::pak_update()"
	Rscript -e \
		"pak::local_system_requirements(execute = TRUE, sudo = FALSE, echo = TRUE)"

rdeps: DESCRIPTION
	Rscript -e \
	"remotes::install_deps(lib = Sys.getenv('R_LIBS_RUNTIME'))"

# hack is the easiest crossplatform way I could think of;
# sed -i won't play nice with macOS
# needs to run twice under some circumstances
roxygenise:
	Rscript -e "roxygen2::roxygenise()"
	Rscript -e "roxygen2::roxygenise()"
	grep -v '^RoxygenNote:' DESCRIPTION > DESCRIPTION_clean
	rm DESCRIPTION
	mv DESCRIPTION_clean DESCRIPTION

check: roxygenise
	Rscript \
		-e "rcmdcheck::rcmdcheck(args = c('--no-manual', '--no-tests'), error_on = 'note' )"

pkgdown: roxygenise
	Rscript -e "pkgdown::build_site(install = TRUE)"

show_pkgdown: pkgdown
	Rscript -e "pkgdown::preview_site(preview = TRUE)"

test: roxygenise
	Rscript -e "testthat::test_local(stop_on_warning = TRUE)"

test-shinytest: install
	Rscript -e \
		"withr::local_envvar(SHINYTEST = 'true'); testthat::test_local(stop_on_warning = TRUE, load_package = 'installed')"

install: roxygenise
	Rscript \
		-e "pak::local_install(dependencies = TRUE)"

smoke-tests: ${smoke_test_jobs}

# see https://stackoverflow.com/a/12110773/3403196
${smoke_test_jobs}: smoke-test-%:
	TAG_FROM_GIT_REF_NAME=$(tag_from_git_ref_name) \
		docker run \
			--rm \
			--entrypoint /bin/bash \
			${REGISTRY_PREFIX}/${IMAGE_OWNER}/${IMAGE_NAME_ROOT}/$*:$(tag_from_git_ref_name) \
			R --vanilla --quiet

health-checks: health-check-rstudio health-check-runner

health-check-rstudio:
	TAG_FROM_GIT_REF_NAME=$(tag_from_git_ref_name) \
		docker compose \
			--file docker-compose.yml \
			--env-file .env \
			up \
			--force-recreate \
			--remove-orphans \
			--detach \
			rstudio
		sleep 3
		curl --fail http://localhost:8787 -o /dev/null || exit 1

health-check-runner:
	TAG_FROM_GIT_REF_NAME=$(tag_from_git_ref_name) \
		docker compose \
			--file docker-compose.yml \
			--env-file .env \
			up \
			--force-recreate \
			--remove-orphans \
			--detach \
			runner
		sleep 3
		curl --fail http://localhost:3838 -o /dev/null || exit 1

bake-devcontainer: bake-developer bake-rstudio

# below 2 targets includes workaround b/c codespace prebuilds cannot access GHCR as per
# https://docs.github.com/en/codespaces/prebuilding-your-codespaces/managing-prebuilds#allowing-a-prebuild-to-access-external-resources
# w/o prebuilds, building takes too long, so we pull from GHCR instead
# this should be removed when GHCR prebuild access works
# there is no canonical way to know whether code is 
# running inside the GHA worker for image creation
# GITHUB_ACTIONS etc. are not defined
# I found this env var by running printenv
# it may change and break this workflow
# can be removed once the whole workaround is obsolete
bake-developer:
ifdef GITHUB_CODESPACE_TOKEN
	TAG_FROM_GIT_REF_NAME=$(tag_from_git_ref_name) \
		docker compose pull developer
	docker tag \
		${REGISTRY_PREFIX}/${IMAGE_OWNER}/${IMAGE_NAME_ROOT}/developer:$(tag_from_git_ref_name) \
		${REGISTRY_PREFIX}/${IMAGE_OWNER}/${IMAGE_NAME_ROOT}/developer:latest
else
	TAG_FROM_GIT_SHA=$(tag_from_git_sha) \
		TAG_FROM_GIT_REF_NAME=$(tag_from_git_ref_name) \
		CAN_PUSH=$(can_push) \
	docker buildx bake \
		--file docker-bake.hcl \
		--file .env \
		developer
endif

bake-rstudio:
ifdef GITHUB_CODESPACE_TOKEN
	TAG_FROM_GIT_REF_NAME=$(tag_from_git_ref_name) \
		docker compose pull rstudio
	docker tag \
		${REGISTRY_PREFIX}/${IMAGE_OWNER}/${IMAGE_NAME_ROOT}/rstudio:$(tag_from_git_ref_name) \
		${REGISTRY_PREFIX}/${IMAGE_OWNER}/${IMAGE_NAME_ROOT}/rstudio:latest
else
	TAG_FROM_GIT_SHA=$(tag_from_git_sha) \
		TAG_FROM_GIT_REF_NAME=$(tag_from_git_ref_name) \
		CAN_PUSH=$(can_push) \
	docker buildx bake \
		--file docker-bake.hcl \
		--file .env \
		rstudio
endif

# this tests whether the devcontainer works using
# https://github.com/devcontainers/cli
devcontainer:
	devcontainer up --workspace-folder .

superlint:
	docker compose up super-linter

lint: actionlint dockerlint dotenv-lint eslint gitleaks jscpd markdownlint \
	rlint sqllint yamllint

actionlint:
	actionlint

dockerlint: 
	hadolint Dockerfile

dotenv-lint:
	dotenv-linter

eslint:
	npx eslint \
		--ignore-path .gitignore \
		--ext .json,js

gitleaks:
	gitleaks detect
	gitleaks protect

jscpd:
	jscpd --silent .

markdownlint:
	markdownlint --dot .

# rlint is *not* just static code analysis, because itruns eval, parse
# so it needs the project runtime (see Dockerfile)
rlint: roxygenise
	Rscript -e "Sys.setenv("LINTR_ERROR_ON_LINT" = TRUE); devtools::load_all(); lintr::lint_package()"

sqllint: sqlfluff sql-lint

sqlfluff:
	sqlfluff lint .

sql-lint:
	sql-lint .

yamllint:
	yamllint .
