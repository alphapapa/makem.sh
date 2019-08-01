#!/bin/bash

# * Safety

# NOTE: These are disabled by default in this template but should be
# enabled when feasible.  Documentation is from the Bash man page.

# ** errexit

# Exit immediately if a pipeline (which may consist of a single simple
# command), a list, or a compound command (see SHELL GRAMMAR above),
# exits with a non-zero status.  The shell does not exit if the
# command that fails is part of the command list immediately following
# a while or until keyword, part of the test following the if or elif
# reserved words, part of any command executed in a && or || list
# except the command follow‐ ing the final && or ||, any command in a
# pipeline but the last, or if the command's return value is being
# inverted with !.  If a compound command other than a subshell
# returns a non-zero status because a command failed while -e was
# being ignored, the shell does not exit.  A trap on ERR, if set, is
# executed before the shell exits.  This option applies to the shell
# environment and each subshell environment separately (see COMMAND
# EXECUTION ENVIRONMENT above), and may cause subshells to exit before
# executing all the commands in the subshell.

# If a compound command or shell function executes in a context where
# -e is being ignored, none of the commands executed within the
# compound command or function body will be affected by the -e
# setting, even if -e is set and a command returns a failure status.
# If a compound command or shell function sets -e while executing in a
# context where -e is ignored, that setting will not have any effect
# until the compound command or the command containing the function
# call completes.

# set -o errexit

# ** nounset

# Treat unset variables and parameters other than the special
# parameters "@" and "*" as an error when performing parameter
# expansion.  If expansion is attempted on an unset variable or
# parameter, the shell prints an error message, and, if not
# interactive, exits with a non-zero status.

# NOTE: When this is not enabled, individual variables can be required
# to be set by using "${var:?}" parameter expansion syntax.

# set -o nounset

# ** pipefail

# If set, the return value of a pipeline is the value of the last
# (rightmost) command to exit with a non-zero status, or zero if all
# commands in the pipeline exit successfully.  This option is disabled
# by default.

# set -o pipefail

# * Elisp

package_initialize=" (progn
(require 'package)
(setq package-archives (list (cons \"gnu\" \"https://elpa.gnu.org/packages/\")
                             (cons \"melpa\" \"https://melpa.org/packages/\")
                             (cons \"melpa-stable\" \"https://stable.melpa.org/packages/\")
                             (cons \"org\" \"https://orgmode.org/elpa/\")))
(package-initialize))"

# * Functions

# ** Emacs

function run_emacs {
    debug "Running: emacs -Q --batch -L \"$load_path\" --eval \"$package_initialize\" $@"

    emacs -Q --batch \
          -L "$load_path" \
          --eval "$package_initialize" \
          "$@"
}

# ** Compilation

function batch-byte-compile {
    debug "batch-byte-compile: $@"

    run_emacs \
        --funcall batch-byte-compile \
        "$@"
}

# ** Files

function elisp_files {
    # Echo list of Elisp files in project.
    git ls-files | egrep "\.el$" | default_file_filter
}

function test_files {
    # Echo list of Elisp test files (files in test and/or tests subdirs).
    elisp_files | default_file_filter | egrep '^tests?/'
}

function default_file_filter {
    # Filter out paths (STDIN) which should be excluded by default.
    egrep -v "(/\.cask/|-autoloads.el)"
}

function load_files_args {
    # For file in $@, echo "--load=$file".

    # NOTE: Putting quotes around the %s causes Emacs to fail to load the
    # file, even though running the command manually like that works.
    for file in "$@"
    do
        printf -- '--load=%s' "$file"
    done
}

# ** Utility

function debug {
    if [[ $debug ]]
    then
        function debug {
            echo "DEBUG: $@" >&2
        }
        debug "$@"
    else
        function debug {
            true
        }
    fi
}
function error {
    echo "ERROR: $@" >&2
    ((errors++))  # Initializes automatically
}
function die {
    error "$@"
    exit $errors
}
function log {
    echo -e "LOG ($(date "+%Y-%m-%d %H:%M:%S")): $@" >&2
}
function usage {
    cat <<EOF
$0 [OPTIONS] ...?

This does something cool!

Options
  -d, --debug  Print debug info
  -h, --help   I need somebody!
EOF
}

# * Rules

# These functions are intended to be called as rules, like a Makefile.

function compile {
    log "Compiling..."

    batch-byte-compile "${byte_compile_files[@]}" \
        || error "Byte-compilation failed."
}

function tests {
    # Run tests.
    test-ert
    test-buttercup
}

function test-buttercup {
    log "Running Buttercup tests..."

    log "Buttercup support not yet implemented."
}

function test-ert {
    # Run ERT tests.
    debug "Test files: ${test_files[@]}"
    debug "Byte-compile files: ${byte_compile_files[@]}"
    debug "Compile: $compile"

    [[ $compile ]] && compile

    log "Running ERT tests..."

    run_emacs \
        $(load_files_args "${test_files[@]}") \
        -f ert-run-tests-batch-and-exit \
        || error "ERT tests failed."
}

# * Defaults

compile=true
load_path="."

byte_compile_files=($(elisp_files))
test_files=($(test_files))

# * Args

args=$(getopt -n "$0" -o dhC -l debug,help,no-compile -- "$@") || { usage; exit 1; }
eval set -- "$args"

while true
do
    case "$1" in
        -d|--debug)
            debug=true
            ;;
        -h|--help)
            usage
            exit
            ;;
        -C|--no-compile)
            unset compile
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

for rule in "${rest[@]}"
do
    if type "$rule" 2>/dev/null | grep "$rule is a function" &>/dev/null
    then
        $rule
    else
        error "Invalid rule: $rule"
    fi
done

exit $errors
