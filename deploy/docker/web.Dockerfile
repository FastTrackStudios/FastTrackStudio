# task-web — static nginx image around a prebuilt dx web bundle.
#
# Artifact-copy style: the wasm build happens OUTSIDE docker (it needs
# the repo's nix dev shell; cargo fetches every git dep itself), then the
# bundle is staged into this directory and copied in. From the repo root:
#
#   # 1. build the bundle WITHOUT TASK_VOX_URL_WEB so the app derives
#   #    its vox URL same-origin from window.location at runtime
#   #    (one env-agnostic image; localhost dev keeps the
#   #    127.0.0.1:18080 default):
#   nix develop .#playwright --command bash -c \
#     'cd apps/web && env -u TASK_VOX_URL_WEB dx build --platform web --release'
#
#   # 2. stage + build (context is deploy/docker, kept tiny):
#   rm -rf deploy/docker/web-public
#   cp -r target/dx/task-app-web/release/web/public deploy/docker/web-public
#   docker build -f deploy/docker/web.Dockerfile \
#     -t codeberg.org/fasttrackstudios/task-web:dev deploy/docker
#
# `web-public/` is gitignored — it only exists at build time.

FROM nginx:alpine

COPY web-nginx.conf /etc/nginx/conf.d/default.conf
COPY web-public/ /usr/share/nginx/html/

EXPOSE 80

HEALTHCHECK --interval=30s --timeout=3s --retries=3 \
    CMD wget -q -O /dev/null http://127.0.0.1:80/ || exit 1
