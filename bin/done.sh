#!/bin/bash

if [ "$(uname)" == "Darwin" ]; then
	# Do something under Mac OS X platform  
	osascript -e 'display notification "hello world!"'
fi
