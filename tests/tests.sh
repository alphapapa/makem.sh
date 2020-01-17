#!/bin/bash

# * makem.sh/tests.sh --- Tests for makem.sh

# https://github.com/alphapapa/makem.sh

# * Commentary:

# Inspired by Damien Cassou's tests for makel:
# <https://gitea.petton.fr/DamienCassou/makel/src/branch/master/test/run-tests.sh>

# * License:

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

# * Functions

function usage {
    cat <<EOF

EOF
}

# ** Utility

# function in-empty-project {
#     # Run $@ in empty project.
#
#     cd "$dir_empty_project" || die
#     debug "IN-EMPTY-PROJECT: $(pwd)  RUNNING: $@"
#     eval "$@"
#     local status=$?
#     cd - &>/dev/null
#     return $status
# }

function cleanup {
    # Remove temporary paths (${paths_temp[@]}).

    for path in "${paths_temp[@]}"
    do
        if [[ $debug ]]
        then
            debug "Debugging enabled: not deleting temporary path: $path"
        elif [[ -r $path ]]
        then
            rm -rf "$path"
        else
            debug "Temporary path doesn't exist, not deleting: $path"
        fi
    done
}
function echo_color {
    # This allows bold, italic, etc. without needing a function for
    # each variation.
    local color_code="COLOR_$1"
    shift

    if [[ $color ]]
    then
        echo -e "${echo_prefix}${!color_code}${@}${COLOR_off}"
    else
        echo "${echo_prefix}$@"
    fi
}
function debug {
    if [[ $debug ]]
    then
        function debug {
            echo_color yellow "DEBUG ($(ts)): $@" >&2
        }
        debug "$@"
    else
        function debug {
            true
        }
    fi
}
function error {
    echo_color red "ERROR ($(ts)): $@" >&2
    ((errors++))
    return 1
}
function die {
    [[ $@ ]] && error "$@"
    exit $errors
}
function log {
    echo "${echo_prefix}LOG ($(ts)): $@" >&2
}
function log_color {
    local color_name=$1
    shift
    echo_color $color_name "LOG ($(ts)): $@" >&2
}
function success {
    if [[ $verbose -ge 2 ]]
    then
        log_color green "$@" >&2
    fi
}
function verbose {
    # $1 is the verbosity level, rest are echoed when appropriate.
    if [[ $verbose -ge $1 ]]
    then
        [[ $1 -eq 1 ]] && local color_name=blue
        [[ $1 -ge 2 ]] && local color_name=cyan

        shift
        log_color $color_name "$@" >&2
    fi
}

function ts {
    date "+%Y-%m-%d %H:%M:%S"
}

# ** Projects

function init-fail-project {
    (
        cd "$dir_fail_project" || die
        git init \
            && echo -e "(cl-first '(0 1))\n(provide 'fail)" >fail.el \
            && git add .  \
            && git commit -m Initial
    ) &>/dev/null \
        || die "Unable to initialize fail project."
}

# ** Testing

function command-output {
    debug "Running: $makem $@"
    (
        debug=$makem_debug
        verbose=$makem_verbose
        source "$makem"
        discover-files
        eval "$@"
    )
}
function should-function-contain {
    local command="$1"
    shift

    verbose 1 "Should contain: $command: $@"
    haystack-has-needles-p "$(command-output "$command")" "$@" \
        && success "Does contain." \
            || error "Does not contain."
}

function haystack-has-needles-p {
    local haystack="$1"
    shift
    for needle in "$@"
    do
        [[ $haystack =~ $needle ]] || return 1
    done
}

function should-function {
    # Ensure makem command $@ returns 0.
    verbose 1 "Should command: $@..."

    (
        debug=$makem_debug
        verbose=$makem_verbose
        source "$makem"
        discover-files
        eval "$@" $makem_output
    )
    local status=$?

    debug "COMMAND: $@  STATUS: $status"
    [[ $status = 0 ]] \
        && success "Command did: $@" \
            || error "Command did not: $@"
}
function should-not-function {
    # Ensure makem command $@ does not return 0.
    verbose 1 "Should NOT command: $@..."

    (
        debug=$makem_debug
        verbose=$makem_verbose
        source "$makem"
        discover-files
        eval "$@" $makem_output
    )
    local status=$?

    debug "COMMAND: $@  STATUS: $status"
    [[ ! $status = 0 ]] \
        && success "Command did not: $@" \
            || error "Command did: $@"
}

function should-function-output {
    verbose 1 "Should output of command: $@"
    debug "Running: $makem $@"

    # Capture output from subshell.
    local output=$( (
                      source "$makem"
                      eval "$@" ) )

    debug "Output for command $@: $output"

    [[ $output ]] \
        && success "Got output for command: $@" \
            || error "No output for command: $@"

}
function should-not-function-output {
    verbose 1 "Should NOT output of command: $@"
    debug "Running: $makem $@"

    # Capture output from subshell.
    local output=$( (
                      source "$makem"
                      eval "$@" ) )

    debug "Output for command $@: $output"

    [[ ! $output ]] \
        && success "No output for command: $@" \
            || error "Got output for command: $@"
}

function should-rule {
    verbose 1 "Should rule: $1"
    debug "Running: $makem $1 (in dir $(pwd))"

    (
        eval "$makem" "$1" $makem_output
    )
    local status=$?
    [[ $status = 0 ]] \
        && success "Rule did: $@" \
            || error "Rule did not: $@"
}
function should-not-rule {
    verbose 1 "Should NOT rule: $1"
    debug "Running: $makem $1 (in dir $(pwd))"

    (
        eval "$makem" "$1" $makem_output
    )
    local status=$?
    [[ ! $status = 0 ]] \
        && success "Rule did NOT: $@" \
            || error "Rule did: $@"
}

# * Rules

# These functions are intended to be called as rules, like a Makefile.
# Some rules test $1 to determine whether the rule is being called
# directly or from a meta-rule; if directly, an error is given if the
# rule can't be run, otherwise it's skipped.

function all {
    verbose 1 "Running all rules..."

    tests
}

function tests {
    verbose 1 "Running all tests..."

    test-fail-project
    test-example-project
}

function test-fail-project {
    # NOTE: The fail project does contain one Elisp file that provides one
    # feature, because makem.sh immediately exits if no such file is found.

    verbose 1 "Running fail project tests..."
    cd "$dir_fail_project" || die

    should-function files-project-source

    should-not-function buttercup-tests-p
    should-not-function ert-tests-p

    should-not-function-output dependencies
    should-not-function-output files-project-test

    should-not-rule all

    should-not-rule lint
    should-not-rule lint-checkdoc
    should-not-rule lint-compile
    should-not-rule lint-indent
    should-not-rule lint-package

    should-not-rule tests
    should-not-rule test-buttercup
    should-not-rule test-ert

    cd - &>/dev/null
}
function test-example-project {
    verbose 1 "Running example project tests..."
    cd "$(dirname "$0")"/../example-package || die "Unable to enter example-package directory."

    should-function-output dependencies
    should-function-output files-project-test

    should-function buttercup-tests-p
    should-function ert-tests-p

    should-function-contain dependencies buttercup dash

    should-function ensure-tests-available Buttercup t

    should-rule compile
    should-rule test-buttercup
    should-rule test-ert

    cd - &>/dev/null
}

# * Colors

COLOR_off='\e[0m'
COLOR_black='\e[0;30m'
COLOR_red='\e[0;31m'
COLOR_green='\e[0;32m'
COLOR_yellow='\e[0;33m'
COLOR_blue='\e[0;34m'
COLOR_purple='\e[0;35m'
COLOR_cyan='\e[0;36m'
COLOR_white='\e[0;37m'

# * Defaults

echo_prefix="${COLOR_purple}MAKEM-TESTS.SH:${COLOR_off} "

makem_output="&>/dev/null"

errors=0
verbose=0

# MAYBE: Disable color if not outputting to a terminal.  (OTOH, the
# colorized output is helpful in CI logs, and I don't know if,
# e.g. GitHub Actions logging pretends to be a terminal.)
color=true

# * Args

args=$(getopt -n "$0" \
              -o dDhv \
              -l debug,debug-makem,debug-load-path,help,verbose,no-color \
              -- "$@") \
    || { usage; exit 1; }
eval set -- "$args"

while true
do
    case "$1" in
        -d|--debug)
            debug=true
            verbose=2
            ;;
        -D|--debug-makem)
            makem_debug=true
            makem_verbose=2
            makem_output="2>&1"
            ;;
        --debug-load-path)
            debug_load_path=true
            ;;
        -h|--help)
            usage
            exit
            ;;
        -v|--verbose)
            ((verbose++))
            args_verbose+=(-v)
            ;;
        --no-color)
            unset color
            ;;
        --)
            # Remaining args (required; do not remove)
            shift
            rest=("$@")
            break
            ;;
    esac

    shift
done

debug "ARGS: $args"
debug "Remaining args: ${rest[@]}"

# * Main

dir_fail_project=$(mktemp -d) || die "Unable to make temporary directory."
paths_temp+=("$dir_fail_project")
init-fail-project

trap cleanup EXIT INT TERM

# Set makem command.
makem="$(readlink -e $(dirname "$0"))/../makem.sh" # ${args_verbose[@]}"

# Run rules.
for rule in "${rest[@]}"
do
    if type -t "$rule" 2>/dev/null | grep function &>/dev/null
    then
        # Pass called-directly as $1 to indicate that the rule is
        # being called directly rather than from a meta-rule.
        $rule called-directly
    elif [[ $rule = test ]]
    then
        # Allow the "tests" rule to be called as "test".  Since "test"
        # is a shell builtin, this workaround is required.
        tests
    else
        error "Invalid rule: $rule"
    fi
done

if [[ $errors -gt 0 ]]
then
    log_color red "Finished with $errors errors."
else
    success "Finished without errors."
fi

exit $errors
