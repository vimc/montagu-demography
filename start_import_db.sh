#!/bin/sh
set -e
MONTAGU_DB_HASH=$(git -C montagu-db rev-parse --short HEAD)
docker run --rm -p 8888:5432 \
       docker.montagu.dide.ic.ac.uk:5000/montagu-db:${MONTAGU_DB_HASH}
