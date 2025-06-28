#!/bin/bash
# Test script to demonstrate different ways to run Unison programs

echo "=== Method 1: Direct run.file (with banner) ==="
ucm run.file hello.u main

echo -e "\n=== Method 2: run.file with stderr redirected ==="
ucm run.file hello.u main 2>/dev/null

echo -e "\n=== Method 3: run.file with banner filtered ==="
ucm run.file hello.u main 2>&1 | sed '1,/^$/d'

echo -e "\n=== Method 4: Using compiled binary (if exists) ==="
if [ -f "$HOME/.unison/v2/unison.sqlite3" ]; then
    echo "Note: First compile in UCM with: compile main hello-compiled"
    echo "Then run: ucm run.compiled hello-compiled.uc"
else
    echo "No codebase found. Initialize with: ucm"
fi