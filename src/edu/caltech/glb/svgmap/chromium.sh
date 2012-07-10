#!/usr/bin/env bash
# Workaround for Chromium bug (https://code.google.com/p/chromium/issues/detail?id=40787)
chromium-browser --allow-file-access-from-files "$(pwd)/svgmap.xhtml" &
