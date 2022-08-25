#!/bin/bash
set -e

#...............................................................................
#                                                                              .
#   https://github.com/rocker-org/rocker-versioned2                            .
#                                                                              .
#...............................................................................

## build ARGs
NCPUS=${NCPUS:--1}

WORKON_HOME=${WORKON_HOME:-/opt/venv}
PYTHON_VENV_PATH=${PYTHON_VENV_PATH:-${WORKON_HOME}/reticulate}
RETICULATE_MINICONDA_ENABLED=${RETICULATE_MINICONDA_ENABLED:-FALSE}

# a function to install apt packages only if they are not installed
function apt_install() {
    if ! dpkg -s "$@" >/dev/null 2>&1; then
        if [ "$(find /var/lib/apt/lists/* | wc -l)" = "0" ]; then
            apt-get update
        fi
        apt-get install -y --no-install-recommends "$@"
    fi
}

apt_install \
    git \
    libpng-dev \
    libpython3-dev \
    python3-dev \
    python3-pip \
    python3-virtualenv \
    python3-venv \
    swig

python3 -m pip --no-cache-dir install --upgrade \
    pip \
    setuptools \
    virtualenv

# Some TF tools expect a "python" binary
if [ ! -e /usr/local/bin/python ]; then
    ln -s "$(which python3)" /usr/local/bin/python
fi

mkdir -p "${WORKON_HOME}"
python3 -m venv "${PYTHON_VENV_PATH}"

pip install pandas
pip install numpy

install2.r --error --skipinstalled -n "$NCPUS" reticulate

## Ensure RStudio inherits this env var
cat <<EOF >>"${R_HOME}/etc/Renviron.site"

WORKON_HOME=${WORKON_HOME}
RETICULATE_MINICONDA_ENABLED=${RETICULATE_MINICONDA_ENABLED}
EOF

## symlink these so that these are available when switching to a new venv
## -f check for file, -L for link, -e for either
if [ ! -e /usr/local/bin/python ]; then
    ln -s "$(which python3)" /usr/local/bin/python
fi

if [ ! -e /usr/local/bin/pip ]; then
    ln -s "${PYTHON_VENV_PATH}/bin/pip" /usr/local/bin/pip
fi

if [ ! -e /usr/local/bin/virtualenv ]; then
    ln -s "${PYTHON_VENV_PATH}/bin/virtualenv" /usr/local/bin/virtualenv
fi

## Allow staff-level users to modify the shared environment
chown -R :staff "${WORKON_HOME}"
chmod g+wx "${WORKON_HOME}"
chown :staff "${PYTHON_VENV_PATH}"

## Enable pyenv
/rocker_scripts/install_pyenv.sh

# Clean up
rm -rf /var/lib/apt/lists/*
rm -rf /tmp/downloaded_packages

## Strip binary installed lybraries from RSPM
## https://github.com/rocker-org/rocker-versioned2/issues/340
strip /usr/local/lib/R/site-library/*/libs/*.so

## Don't use OpenBLAS with reticulate
## https://github.com/rocker-org/rocker-versioned2/issues/471
if R -q -e 'sessionInfo()' | grep -q openblas; then
    ARCH=$(uname -m)
    echo "Switching BLAS"
    update-alternatives --set "libblas.so.3-${ARCH}-linux-gnu" "/usr/lib/${ARCH}-linux-gnu/blas/libblas.so.3"
    update-alternatives --set "liblapack.so.3-${ARCH}-linux-gnu" "/usr/lib/${ARCH}-linux-gnu/lapack/liblapack.so.3"
fi

# Check Python version
echo -e "Check the Python to use with reticulate...\n"

R -q -e 'reticulate::py_discover_config(required_module = NULL, use_environment = NULL)'

echo -e "\nInstall Python, done!"
