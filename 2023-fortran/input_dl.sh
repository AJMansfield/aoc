#!/bin/bash
script_dir=$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )

usage () {
    printf "Usage: %s: -y ${year:-YEAR} -d ${day:-DAY} -o ${output:-OUTPUT} -S TOKEN_FILE\n" "$0"
    printf "Downloads your customized input using your personalized token to use as input.txt."
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

url="https://adventofcode.com/${year}/day/${day}/input"

echo "retrieving ${url}" >&2
curl --silent --compressed \
 --user-agent "AJMansfield's AOC Script +mailto:anson.mansfield@gmail.com +https://github.com/AJMansfield/aoc/blob/master/2023-fortran/input_dl.sh" \
 -H "Cookie: session=${session_token}" \
 -o "${output}" \
 -- "${url}"
 