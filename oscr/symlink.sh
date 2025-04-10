#!/bin/bash

set -e

NAME="com.example.oscr.xrnx"

VERSION="$1"

if [ "${VERSION}" == "" ]; then
  echo "Need Renoise version (e.g. 3.4.4)"
  exit 1
fi

set -u

REPO="$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")"

if [ "$(uname -s)" == "Darwin" ]; then
  PLUG_DIR="${HOME}/Library/Preferences/Renoise/V${VERSION}/Scripts/Tools"

else
  PLUG_DIR="${HOME}/.config/Renoise/V${VERSION}/Scripts/Tools"
fi

if [ -e "${PLUG_DIR}/${NAME}" ]; then
  echo "Plugin already symlinked, skipping"
  exit 0
elif [ ! -d "${PLUG_DIR}" ]; then
  echo "Missing plugin directory ${PLUG_DIR}, aborting"
  exit 1
else
  echo "Symlinking plugin"
  cd "${PLUG_DIR}" && ln -s "${REPO}/plugin" "${NAME}"
fi
