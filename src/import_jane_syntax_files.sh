#!/bin/bash
# First, cd into the directory containing the script so the files
# are imported to the right place regardless of where the script is
# run from.
cd "$(dirname "$(realpath -- "$0")")"

if [[ "$1" == "--help" || "$2" == "--help" || "$#" -gt 2 ]]; then
  echo "Usage: $0 [optional_branch] [optional_url]"
  echo "Download the Jane Syntax files from a GitHub repository."
  echo "If a branch and remote are not provided, it will default to the 'main' branch and the flambda-backend GitHub repository."
  exit 1
fi

BRANCH=${1:-"main"}
REMOTE=${2:-"https://github.com/ocaml-flambda/flambda-backend.git"}
FILE_PATHS=(
  "ocaml/parsing/jane_syntax.ml"
  "ocaml/parsing/jane_syntax.mli"
  "ocaml/parsing/jane_syntax_parsing.ml"
  "ocaml/parsing/jane_syntax_parsing.mli"
  "ocaml/parsing/jane_asttypes.ml"
  "ocaml/parsing/jane_asttypes.mli"
  "ocaml/utils/language_extension_kernel.mli"
  "ocaml/utils/language_extension_kernel.ml"
)

temp_repo=$(mktemp -d)
git clone --depth 1 --branch "$BRANCH" "$REMOTE" "$temp_repo"

echo "$REMOTE $(git -C "$temp_repo" rev-parse HEAD)" > imported-commit.txt

for file_path in "${FILE_PATHS[@]}"; do
  {
    # Disable the warning for record patterns that don't list all their fields.
    # It's disabled in the compiler.
    echo '(*_ This file is manually imported from the Jane Street version of the'
    echo '   OCaml compiler. Don'\''t make changes directly to this file. *)'
    echo '[@@@ocaml.warning "-missing-record-field-pattern"]'
    echo 'open! Shadow_compiler_distribution'
    echo
    sed < "$temp_repo/$file_path" 's/(\* CR/(* JS-only/g'
  } > "$(basename "$file_path")"
done

rm -rf "$temp_repo"
