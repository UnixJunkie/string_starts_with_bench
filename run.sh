#!/bin/bash

set -x

obuild configure
obuild build
./test
