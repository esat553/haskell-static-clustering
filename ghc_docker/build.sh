#!/usr/bin/env bash
set -e

if [ -z "$1" ]; then
  echo "Fehler: Bitte geben Sie eine GHC-Version als Argument an."
  echo "Beispiel: $0 9.4.8"
  exit 1
fi

GHC_VERSION="$1"
IMAGE_NAME="safe-docker-ghc${GHC_VERSION}"

echo "🔨 Baue Image '${IMAGE_NAME}' mit GHC ${GHC_VERSION}..."

docker build \
  --build-arg GHC_VERSION="${GHC_VERSION}" \
  --tag "${IMAGE_NAME}" \
  --no-cache \
  .

echo "✅ Image ${IMAGE_NAME} erfolgreich gebaut."