variable "TAG_FROM_GIT_SHA" {
  default = "latest"
}
variable "TAG_FROM_GIT_REF_NAME" {
  default = "latest"
}
// only CI should push images and caches
// you *could* push from a local machine in an emergency,
// by setting this to "true" in the `docker buildx bake` command
variable "CAN_PUSH" {
  default = false
}

// every image should be tagged with sha and ref name
// both default to "latest" when unset, so it's the same in that case
function "compose_tags_field" {
  params = [image_name_stage]
  result = [
    "${REGISTRY_PREFIX}/${IMAGE_OWNER}/${IMAGE_NAME_ROOT}/${image_name_stage}:${TAG_FROM_GIT_SHA}",
    "${REGISTRY_PREFIX}/${IMAGE_OWNER}/${IMAGE_NAME_ROOT}/${image_name_stage}:${TAG_FROM_GIT_REF_NAME}"
  ]
}

function "compose_cache_to_field" {
  params = [image_name_stage]
  result = flatten([concat([
    "${CAN_PUSH}" ?
      "type=registry,ref=${REGISTRY_PREFIX}/${IMAGE_OWNER}/${IMAGE_NAME_ROOT}/${image_name_stage}/cache:${TAG_FROM_GIT_REF_NAME},mode=max" :
      "type=inline"
  ])])
}

// caches first in the below order are tried first
// 1) try the git ref in question
// 2) try main as the backup source
// 3) cache fail (no problem, just gets rebuild)
// notice that caching from some branch locally may lead to a cache miss on GHCR
// because some local branch may not exist on GHCR
function "compose_cache_from_field" {
  params = [image_name_stage]
  result = [
    "type=registry,ref=${REGISTRY_PREFIX}/${IMAGE_OWNER}/${IMAGE_NAME_ROOT}/${image_name_stage}/cache:${TAG_FROM_GIT_REF_NAME}",
    "type=registry,ref=${REGISTRY_PREFIX}/${IMAGE_OWNER}/${IMAGE_NAME_ROOT}/${image_name_stage}/cache:main"
  ]
}

group "default" {
  targets = [
    "from-r-ver"
  ]
}

group "from-r-ver" {
  targets = [
    "builder",
    "developer"
  ]
}

group "from-rstudio" {
  targets = [
    "rstudio"
  ]
}

// get nice labels from the github context
// tags are NOT taken from here!
// see https://github.com/docker/metadata-action
target "docker-metadata-action" {}

target "default" {
  inherits = [
    "docker-metadata-action"
  ]
  platforms = [
    "linux/amd64"
  ]
  args = {
    RSPM_SNAPSHOT_DATE = "${RSPM_SNAPSHOT_DATE}",
    GITHUB_SHA = "${TAG_FROM_GIT_SHA}",
    GITHUB_REF_NAME = "${TAG_FROM_GIT_REF_NAME}"
  }
  output = CAN_PUSH ? ["type=registry"] : ["type=docker"]
}

target "builder" {
  inherits = [
    "default"
  ]
  target = "builder"
  cache-from = compose_cache_from_field("builder")
  cache-to = compose_cache_to_field("builder")
  tags = compose_tags_field("builder")
}

target "developer" {
  inherits = [
    "default"
  ]
  target = "developer"
  cache-from = compose_cache_from_field("developer")
  cache-to = compose_cache_to_field("developer")
  tags = compose_tags_field("developer")
}

target "rstudio" {
  inherits = [
    "default"
  ]
  args = {
    ROCKER_VARIANT = "rstudio",
    RSPM_SNAPSHOT_DATE = "${RSPM_SNAPSHOT_DATE}",
  }
  target = "developer"
  cache-from = compose_cache_from_field("rstudio")
  cache-to = compose_cache_to_field("rstudio")
  tags = compose_tags_field("rstudio")
}

target "runner" {
  inherits = [
    "default"
  ]
  target = "runner"
  cache-from = compose_cache_from_field("runner")
  cache-to = compose_cache_to_field("runner")
  tags = compose_tags_field("runner")
}
