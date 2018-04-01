#!/usr/bin/env bash

# Constants
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NO_COLOR='\033[0m'
OS_TYPE=$(uname | tr '[:upper:]' '[:lower:]')
ROOT_DIR=$(dirname $(realpath $0))
TEMPLATES_DIR=$(printf "$ROOT_DIR/templates")

# Functions

# Print info message
# $1: the info message
function info {
    printf "$GREEN$1$NO_COLOR\n"
}

# Print warning message
# $1: the warning message
function warn {
    printf "$YELLOW$1$NO_COLOR\n"
}

# Print error message
# $1: error message
function error {
    printf "$RED$1$NO_COLOR\n"
}

# Link templates
# $1: template pattern
function linkTemplates {
    for file in $(find $TEMPLATES_DIR -type f | sed "s|$TEMPLATES_DIR/||"); do
        if [[ $file =~ (.+)_$1(\.[^\.]+|())$ ]]; then  # Files for $1
            dest=$(printf "$HOME/.${BASH_REMATCH[1]}${BASH_REMATCH[2]}")
        elif [[ $file =~ (.*)_(darwin|linux)(\.[^\.]+|())$ ]]; then  # Ignore files for other OS
            continue
        elif [[ $file =~ (.+)(\.[^\.]+|())$ ]]; then  # Common files for all OS
            dest=$(printf "$HOME/.${BASH_REMATCH[1]}${BASH_REMATCH[2]}")
        fi

        dir=$(dirname $file)
        if [ "$dir" != "." ]; then
            mkdir -p $HOME/.$dir
        fi

        ln -f $TEMPLATES_DIR/$file $dest
        info "templates/$file -> $dest"
    done
}

linkTemplates "${OS_TYPE}"
