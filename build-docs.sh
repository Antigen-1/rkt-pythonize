#!/bin/sh
# README.md and CHANGELOG.md are generated: edit the Scribble sources under
# scribblings/ and run this script.  scribblings/readme.scrbl and
# scribblings/changelog.scrbl are chapters of the manual and Markdown files of
# their own: each is rendered on its own here.
set -e
cd "$(dirname "$0")"

tmp=$(mktemp -d "${TMPDIR:-/tmp}/rkt-pythonize-docs.XXXXXX")
trap 'rm -rf "$tmp"; rm -f README.md.new CHANGELOG.md.new' 0 1 2 15

raco scribble --markdown --dest "$tmp" scribblings/readme.scrbl
raco scribble --markdown --dest "$tmp" scribblings/changelog.scrbl

for chapter in readme changelog; do
    if [ ! -s "$tmp/$chapter.md" ]; then
        echo "build-docs.sh: raco scribble wrote no $tmp/$chapter.md, or wrote it empty" >&2
        exit 1
    fi
done

# A chapter rendered on its own is a document of its own, so the Markdown
# backend writes its heading as the title line: "# README".  That heading
# becomes the file's own top-level heading, and the body after it is copied
# unchanged.  ("## README" and "# ## README" are accepted too, in case the
# chapter is ever a section rather than a title.)
#
# The text is written beside the file it belongs to and then renamed onto it:
# a rename is atomic, so a failure leaves the file as it was, and nothing is
# ever written onto it directly.  $1 rendered chapter, $2 the heading written
# for it, $3 the file's own top-level heading, $4 the file to write.
rehead () {
    if ! awk -v written="$2" -v top="$3" '
             !done && ($0 == "# " written || $0 == "## " written || $0 == "# ## " written) {
                 print top
                 done = 1
                 next
             }
             { print }
             END { if (!done) exit 1 }' "$1" > "$4.new"
    then
        rm -f "$4.new"
        echo "build-docs.sh: no '# $2' heading in $1" >&2
        exit 1
    fi
    if [ ! -s "$4.new" ]; then
        rm -f "$4.new"
        echo "build-docs.sh: the text for $4 came out empty" >&2
        exit 1
    fi
    mv -f "$4.new" "$4"
}

rehead "$tmp/readme.md" README "# rkt-pythonize" README.md
rehead "$tmp/changelog.md" Changelog "# Changelog" CHANGELOG.md
