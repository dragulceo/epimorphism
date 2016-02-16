#!/usr/bin/env bash

pulp --watch --before clear --then './browserify.sh' build
