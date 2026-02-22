#!/usr/bin/env bash

set -e

sbt clean
sbt Test/compile
sbt test
sbt publishSigned
sbt sonatypeBundleRelease