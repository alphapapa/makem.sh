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

# These functions return a path to an elisp file which can be loaded
# by Emacs on the command line with -l or --load.

function elisp-checkdoc-file {
    # Since checkdoc doesn't have a batch function that exits non-zero
    # when errors are found, we make one.
    local file=$(mktemp)

    cat >$file <<EOF
(require 'cl-lib)

(defvar makem-checkdoc-errors-p nil)

(defun makem-checkdoc-files-and-exit ()
  "Run checkdoc-file on files remaining on command line, exiting non-zero if there are warnings."
  (let* ((files (cl-loop for arg in command-line-args-left
                         collect (expand-file-name arg)))
         (checkdoc-create-error-function
          (lambda (text start end &optional unfixable)
            (let ((msg (concat (checkdoc-buffer-label) ":"
                               (int-to-string (count-lines (point-min) (or start (point-min))))
                               ": " text)))
              (message msg)
              (setq makem-checkdoc-errors-p t)
              (list text start end unfixable)))))
    (mapcar #'checkdoc-file files)
    (when makem-checkdoc-errors-p
      (kill-emacs 1))))

(makem-checkdoc-files-and-exit)
EOF
    echo $file
}

function elisp-package-initialize-file {
    local file=$(mktemp)

    cat >$file <<EOF
(require 'package)
(setq package-archives (list (cons "gnu" "https://elpa.gnu.org/packages/")
                             (cons "melpa" "https://melpa.org/packages/")
                             (cons "melpa-stable" "https://stable.melpa.org/packages/")
                             (cons "org" "https://orgmode.org/elpa/")))
(package-initialize)
(setq load-prefer-newer t)
EOF
    echo $file
}

# * Functions

# ** Emacs

function run_emacs {
    debug "Running: emacs -Q --batch -L \"$load_path\" --load=$package_initialize_file $@"

    output_file=$(mktemp)
    emacs -Q --batch -L "$load_path" \
          --load=$package_initialize_file \
          "$@" \
        &>$output_file

    exit=$?
    [[ $exit != 0 ]] && debug "Emacs exited non-zero: $exit"
    if [[ $verbose -gt 1 || $exit != 0 ]]
    then
        cat $output_file
    fi
    rm -f $output_file

    return $exit
}

# ** Compilation

function batch-byte-compile {
    debug "batch-byte-compile: $@"

    run_emacs \
        --funcall batch-byte-compile \
        "$@"
}

# ** Files

function project-elisp-files {
    # Echo list of Elisp files in project.
    git ls-files | egrep "\.el$" | exclude-files
}

function project-elisp-files-non-test {
    # Echo list of Elisp files that are not tests.
    project-test-files invert
}

function project-test-files {
    # Echo list of Elisp test files (files in test and/or tests
    # subdirs).  If $1 is non-empty, return non-test files.
    local invert
    [[ $1 ]] && invert="-v"

    project-elisp-files | exclude-files | egrep $invert '^tests?/'
}

function exclude-files {
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

function files_args {
    # For file in STDIN, echo "$file".
    while read file
    do
        printf -- '%q ' "$file"
    done
}

# ** Utility

function cleanup {
    # Remove temporary paths (${temp_paths[@]}).

    for path in "${temp_paths[@]}"
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
    if [[ $color ]]
    then
        local color_code="COLOR_$1"
        shift

        echo -e "${!color_code}${@}${COLOR_off}"
    else
        echo "$@"
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
}
function die {
    error "$@"
    exit $errors
}
function log {
    echo "LOG ($(ts)): $@" >&2
}
function log_color {
    local color=$1
    shift
    echo_color $color "LOG ($(ts)): $@" >&2
}
function verbose {
    # $1 is the verbosity level, rest are echoed when appropriate.
    if [[ $verbose -ge $1 ]]
    then
        local color
        [[ $1 = 1 ]] && color=blue
        [[ $1 = 2 ]] && color=cyan

        shift
        log_color $color "$@" >&2
    fi
}

function ts {
    date "+%Y-%m-%d %H:%M:%S"
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

function all {
    verbose 1 "Running all rules..."

    lint
    tests
}

function compile {
    verbose 1 "Compiling..."

    batch-byte-compile "${project_byte_compile_files[@]}" \
        || error "Byte-compilation failed."
}

function lint {
    verbose 1 "Linting..."

    lint-checkdoc
    lint-package
}

function lint-checkdoc {
    verbose 1 "Linting checkdoc..."

    local checkdoc_file=$(elisp-checkdoc-file)
    temp_paths+=($checkdoc_file)

    run_emacs \
        --load=$checkdoc_file \
        $(project-elisp-files-non-test) \
        || error "Linting checkdoc failed."
}

function lint-package {
    verbose 1 "Linting package..."

    run_emacs \
        --eval "(require 'package-lint)" \
        --funcall package-lint-batch-and-exit \
        $(project-elisp-files-non-test | files_args) \
        || error "Package linting failed."
}

function tests {
    # Run tests.
    test-ert
    test-buttercup
}

function test-buttercup {
    verbose 1 "Running Buttercup tests..."

    verbose 1 "Buttercup support not yet implemented."
}

function test-ert {
    # Run ERT tests.
    debug "Test files: ${project_test_files[@]}"
    debug "Byte-compile files: ${project_byte_compile_files[@]}"
    debug "Compile: $compile"

    [[ $compile ]] && compile

    verbose 1 "Running ERT tests..."

    run_emacs \
        $(load_files_args "${project_test_files[@]}") \
        -f ert-run-tests-batch-and-exit \
        || error "ERT tests failed."
}

# * Defaults

# TODO: Disable color if not outputting to a terminal.
color=true
errors=0
verbose=0

compile=true
load_path="."

project_byte_compile_files=($(project-elisp-files))
project_test_files=($(project-test-files))

package_initialize_file=$(elisp-package-initialize-file)
temp_paths+=($package_initialize_file)

# ** Colors

COLOR_off='\e[0m'       # Text Reset
COLOR_black='\e[0;30m'        # Black
COLOR_red='\e[0;31m'          # Red
COLOR_green='\e[0;32m'        # Green
COLOR_yellow='\e[0;33m'       # Yellow
COLOR_blue='\e[0;34m'         # Blue
COLOR_purple='\e[0;35m'       # Purple
COLOR_cyan='\e[0;36m'         # Cyan
COLOR_white='\e[0;37m'        # White

# * Args

args=$(getopt -n "$0" -o dhvC -l debug,help,verbose,no-color,no-compile -- "$@") || { usage; exit 1; }
eval set -- "$args"

while true
do
    case "$1" in
        -d|--debug)
            debug=true
            verbose=2
            ;;
        -h|--help)
            usage
            exit
            ;;
        -v|--verbose)
            ((verbose++))
            ;;
        --no-color)
            unset color
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

trap cleanup EXIT INT TERM

for rule in "${rest[@]}"
do
    if type "$rule" 2>/dev/null | grep "$rule is a function" &>/dev/null
    then
        $rule
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
elif [[ $verbose ]]
then
    verbose 1 "Finished with $errors errors."
fi

exit $errors
