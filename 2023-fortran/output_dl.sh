#!/bin/bash
script_dir=$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )

usage () {
    printf "Usage: %s: -y ${year:-YEAR} -d ${day:-DAY} -o ${output:-OUTPUT} -S TOKEN_FILE\n" "$0"
    exit 1
}

while getopts y:d:o:s:S: flag
do
    case $flag in
    y) year="$OPTARG" ;;
    d) day=$((10#$OPTARG)) ;; # remove leading zero if needed
    o) output="$OPTARG" ;;
    s) session_token="$OPTARG" ;;
    S) session_token=$(cat "$OPTARG") ;;
    ?) usage $@ ;;
    esac
done

[ -z ${year} ] && usage
[ -z ${day} ] && usage
[ -z ${output} ] && usage
[ -z ${session_token} ] && usage

url="https://adventofcode.com/${year}/day/${day}"

temp_doc="$(mktemp)"
trap 'rm -rf -- "$temp_doc"' EXIT

echo "retrieving ${url}" >&2
curl --compressed \
 --user-agent 'AOC-Test-Runner-Bot +https://github.com/AJMansfield/aoc/blob/master/2023-fortran/' \
 -H "Cookie: session=${session_token}" \
 -o "${temp_doc}" \
 -- "${url}" \
|| exit $?

< "${temp_doc}" \
  grep -Po '(?<=<p>Your puzzle answer was <code>)\d*(?=</code>.</p>)' \
| awk '$0="Part "NR": "$0' \
> "${output}"

# part1_sel='body > main:nth-child(3) > p:nth-child(2) > code:nth-child(1)'
# part2_sel='body > main:nth-child(3) > p:nth-child(4) > code:nth-child(1)'

# cat $temp_doc

# xpath -e "${part1_xpath}" -e "${part2_xpath}" "${temp_doc}"