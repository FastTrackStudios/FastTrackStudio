# task-ui-lab — static nginx image around a prebuilt vite dist.
#
# Artifact-copy style, same as web.Dockerfile. From the repo root:
#
#   (cd ui-lab && pnpm install --frozen-lockfile && pnpm build)
#   rm -rf deploy/docker/ui-lab-dist
#   cp -r ui-lab/dist deploy/docker/ui-lab-dist
#   docker build -f deploy/docker/ui-lab.Dockerfile \
#     -t codeberg.org/fasttrackstudios/task-ui-lab:dev deploy/docker
#
# `ui-lab-dist/` is gitignored — it only exists at build time.

FROM nginx:alpine

COPY ui-lab-nginx.conf /etc/nginx/conf.d/default.conf
COPY ui-lab-dist/ /usr/share/nginx/html/

EXPOSE 80

HEALTHCHECK --interval=30s --timeout=3s --retries=3 \
    CMD wget -q -O /dev/null http://127.0.0.1:80/ || exit 1
