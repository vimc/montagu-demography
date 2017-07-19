#!/usr/bin/env bash
set -x

# This is derived from montagu-legacy-data:scripts/teamcity-import.sh

MONTAGU_DB_TAG=$(git -C montagu-db rev-parse --short HEAD)

WORKDIR=/import

MONTAGU_DB_HOST=montagu-db-server
MONTAGU_DB_IMAGE=docker.montagu.dide.ic.ac.uk:5000/montagu-db:${MONTAGU_DB_TAG}
MONTAGU_IMPORT_IMAGE=docker.montagu.dide.ic.ac.uk:5000/montagu-db-import:${MONTAGU_DB_TAG}

MONTAGU_DB_NETWORK=montagu-db-import-nw

NETWORK=$(docker network create $MONTAGU_DB_NETWORK)
docker pull $MONTAGU_DB_IMAGE || exit 1
docker pull $MONTAGU_IMPORT_IMAGE || exit 1

docker run -d --rm \
       --name $MONTAGU_DB_HOST \
       --network $MONTAGU_DB_NETWORK \
       $MONTAGU_DB_IMAGE

# First wait for the container to come up; for now this should be enough
sleep 1

docker ps

docker run --rm \
       --network $MONTAGU_DB_NETWORK \
       -e MONTAGU_DB_HOST=$MONTAGU_DB_HOST \
       -e MONTAGU_DB_PORT=5432 \
       -v ${PWD}:${WORKDIR} \
       -w ${WORKDIR} \
       ${MONTAGU_IMPORT_IMAGE} \
       Rscript --no-environ import.R

SUCCESS=$?

if [ $SUCCESS -eq 0 ]; then
    echo "Success!"
    docker exec $MONTAGU_DB_HOST \
           pg_dump -U vimc \
             -t demographic_statistic \
             -t gender \
             -t projection_variant \
             -t demographic_statistic_type \
             -t source \
             -Fc montagu \
             > montagu.dump
    SUCCESS=$?
    if [ $SUCCESS -eq 0 ]; then
        echo "Export success"
    else
        echo "Export failed"
    fi
else
    echo "Failure :("
fi

docker stop $MONTAGU_DB_HOST
docker network rm $MONTAGU_DB_NETWORK

exit $SUCCESS
