#!/usr/bin/env bash

set -u

resourceReportSuffix=_utilization_placed.rpt
timingReportSuffix=_timing_summary_routed.rpt

extract-common-prefixes() {
  declare -n filePaths="$1"
  declare -n ret="$2"
  declare -a nonUniquePrefixes=()

  for filePath in "${filePaths[@]}"; do
    filePath="$(basename "$filePath")"
    if [[ "$filePath" == *"$resourceReportSuffix" ]]; then
      nonUniquePrefixes+=("${filePath%"$resourceReportSuffix"}")
    fi
#      nonUniquePrefixes+=("${filePath%"$timingReportSuffix"}")
#    fi
  done

  ret=($(for prefix in "${nonUniquePrefixes[@]}"; do
    echo "$prefix"
  done | uniq))
}

extract-resource-line() {
  local file="$1"
  local linePattern="$2"
  local field="$3"
  cat "$file" | grep "$linePattern" | head -1 | cut -d '|' -f "$field" | xargs echo
}
extract-resource-usage() {
  local kind="$1"
  local file="$2"

  local linePattern=

  case "$kind" in
    luts) linePattern='CLB LUTs';;
    registers) linePattern='CLB Registers';;
    brams) linePattern='| Block RAM Tile';;
    dsps) linePattern='DSPs'
  esac

  local used="$(extract-resource-line "$file" "$linePattern" 3)"
  local available="$(extract-resource-line "$file" "$linePattern" 6)"
  echo "$used"/"$available"
}
extract-wns() {
  local file="$1"
  sed -rn -e '
    /.*WNS\(ns\).*/{
      n
      n
      s/^[^c]*clk\s*(\S+)\s.+$/\1/
      T
      p
    }' "$file"
}


if [ $# -lt 1 ]; then
  echo "Usage: $0 <result-folders...>" >&2
  exit 2
fi

if [ "$1" = extract ]; then
  extract-resource-usage "$2" "$3"
  exit
fi

for folder in "$@"; do
  declare -a files=("$folder"/*)
  declare -a prefixes=()

  echo -"$(basename "$folder")"

  extract-common-prefixes files prefixes

#  declare -p folder >&2
#  declare -p prefixes >&2

  for prefix in "${prefixes[@]}"; do
    resourceReport="$folder"/"${prefix}${resourceReportSuffix}"
    timingReport="$folder"/"${prefix}${timingReportSuffix}"
    echo "$prefix"
    extract-resource-usage luts "$resourceReport"
    extract-resource-usage registers "$resourceReport"
    extract-resource-usage brams "$resourceReport"
    extract-resource-usage dsps "$resourceReport"
    extract-wns "$timingReport"
  done
done | python -c '
from __future__ import print_function
import sys

def print_table_header(title):
  print("<tr>")
  print(
    "<td bgcolor=\"#444444\"><font color=\"#FFFFFF\"><b>{}</b></font></td>".\
      format(title))
  print("<td><b>LUTs</b></td>")
  print("<td><b>Registers</b></td>")
  print("<td><b>BRAMs</b></td>")
  print("<td><b>DSPs</b></td>")
  print("<td><b>WNS (ns)</b></td>")
  print("</tr>")

print("<table>")
while True:
  line = sys.stdin.readline()
  if len(line) == 0:
    break
  line = line[:-1]
  if line.startswith("-"):
    print_table_header(line[1:])
    continue
  else:
    print("<tr>")
    print("<td><b>{}</b></td>".format(line))

  for _ in range(5):
    print("<td>{}</td>".format(sys.stdin.readline()[:-1]))
  print("</tr>")
print("</table>")
'

#fileName='2018-06-30-03-05-40/hls_fft_utilization_placed.rpt'
#
#lutsUsed="$(cat "$fileName" | grep 'Slice LUTs' | cut -d '|' -f 3 | xargs echo)"
#lutsAvailable="$(cat "$fileName" | grep 'Slice LUTs' | cut -d '|' -f 5 | xargs echo)"
#echo LUTs "$lutsUsed"/"$lutsAvailable"
#
#registersUsed="$(cat "$fileName" | grep 'Slice Registers' | cut -d '|' -f 3 | xargs echo)"
#registersAvailable="$(cat "$fileName" | grep 'Slice Registers' | cut -d '|' -f 5 | xargs echo)"
#echo Registers "$registersUsed"/"$registersAvailable"
#
#bramsUsed="$(cat "$fileName" | grep '| Block RAM Tile' | cut -d '|' -f 3 | xargs echo)"
#bramsAvailable="$(cat "$fileName" | grep '| Block RAM Tile' | cut -d '|' -f 5 | xargs echo)"
#echo BRAMs "$bramsUsed"/"$bramsAvailable"

#TODO dsps
