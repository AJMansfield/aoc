#!/bin/bash
script_dir=$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )

usage () {
    printf "Usage: %s: -y ${year:-YEAR} -d ${day:-DAY} -S TOKEN_FILE\n" "$0"
    exit 1
}

while getopts y:d:s:S: flag
do
    case $flag in
    y) year="$OPTARG" ;;
    d) day="$OPTARG" ;;
    s) session_token="$OPTARG" ;;
    S) session_token=$(cat "$OPTARG") ;;
    ?) usage $@ ;;
    esac
done

[ -z ${year} ] && usage
[ -z ${day} ] && usage
[ -z ${session_token} ] && usage

printf "https://adventofcode.com/${year}/day/${day}/input\n"
curl --compressed --user-agent 'input-dl.sh +http://'
#  -H 'User-Agent: input-dl.sh'\
#  -H 'Accept: text/plain'\
#  -H 'Accept-Encoding: gzip, deflate, br'\
#  -H "Referer: https://adventofcode.com/${year}/day/${day}"\
#  -H "Cookie: ${session}"\
#  -H 'Sec-Fetch-Dest: document'\
#  -H 'Sec-Fetch-Site: none'\
#  -H 'TE: trailers'\
#  -- "https://adventofcode.com/${year}/day/${day}/input"