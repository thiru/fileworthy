#!/bin/sh

echo Building fileworthy executable...
echo

ros dump --remove-docstrings --delete-debug-info executable fileworthy.ros

echo
echo fileworthy build complete
