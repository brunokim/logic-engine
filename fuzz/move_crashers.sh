#!/bin/sh

set +x

rm ./crashers/*.output
rm ./crashers/*.quoted

mv ./crashers/* ./corpus

set -x
