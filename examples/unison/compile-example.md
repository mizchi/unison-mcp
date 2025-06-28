# Compiling Unison Programs

## Overview

Unison provides the `compile` command to create executable binaries from your Unison programs. This allows you to run programs without the UCM welcome banner.

## Steps to Compile

1. First, ensure you have the base library installed:
```
ucm> lib.install @unison/base/releases/3.21.0
```

2. Load your Unison file in UCM:
```
ucm> load examples/unison/hello.u
```

3. Add the definitions to your codebase:
```
ucm> add
```

4. Compile the program:
```
ucm> compile main hello-executable
```

This creates a file `hello-executable.uc` in your codebase directory.

## Running Compiled Programs

Run the compiled program without the UCM banner:
```bash
ucm run.compiled hello-executable.uc
```

Or with arguments:
```bash
ucm run.compiled hello-executable.uc Alice
```

## Alternative: Direct Execution

While `ucm run.file` shows the banner:
```bash
ucm run.file hello.u main Alice
```

The compiled version runs cleaner:
```bash
ucm run.compiled hello-executable.uc Alice
```

## Notes

- The `.uc` file contains bytecode and all dependencies
- Compiled programs start faster than interpreted ones
- You can distribute `.uc` files as standalone executables (with UCM runtime)
- The `run.compiled` command still requires UCM but skips the interactive mode