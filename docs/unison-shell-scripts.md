# Running Unison Programs as Shell Scripts

## Suppressing the UCM Welcome Banner

Currently, UCM doesn't have a built-in flag to suppress the welcome banner. However, there are several workarounds:

### Method 1: Using Compiled Binaries (Cleanest Output)

1. Compile your program in UCM:
```bash
ucm> compile main myprogram
```

2. Run the compiled binary:
```bash
ucm run.compiled myprogram.uc
```

This produces the cleanest output with minimal UCM overhead.

### Method 2: Output Redirection

For scripts that need clean output, redirect stderr:
```bash
ucm run.file hello.u main 2>/dev/null
```

Or filter out the banner lines:
```bash
ucm run.file hello.u main 2>&1 | tail -n +15
```

### Method 3: Wrapper Script with Filtering

Create a wrapper that filters the banner:
```bash
#!/bin/bash
# unison-run - Clean Unison runner

# Run UCM and filter out the banner
ucm run.file "$@" 2>&1 | sed '1,/^$/d'
```

Unison provides several ways to execute programs from the command line, though it doesn't support traditional shebang (`#!`) execution in the same way as Python or Ruby.

## Methods for Running Unison Programs

### 1. Using `ucm run.file`

The most direct way to run a Unison file from the command line:

```bash
ucm run.file hello.u main
```

This command:
- Loads the file `hello.u`
- Executes the function `main`
- Passes any additional arguments to the program

Example Unison file (`hello.u`):
```unison
hello : base.Text -> base.Text
hello name = base.Text.++ "Hello, " (base.Text.++ name "!")

main : '{base.IO, base.Exception} ()
main = do
  args = base.io.cli.getArgs ()
  name = base.data.List.head args |> base.data.Optional.getOrElse "World"
  base.io.printLine (hello name)
```

### 2. Using Compiled Binaries

Compile your Unison program to a standalone executable:

```bash
# Inside UCM
ucm> compile main myProgram

# From command line
ucm run.compiled myProgram.uc -- arg1 arg2
```

### 3. Wrapper Script with Shebang

Since `ucm run.file` requires multiple arguments, create a wrapper script:

```bash
#!/bin/bash
# Save as: unison-script
ucm run.file "$1" "${2:-main}" "${@:3}"
```

Then create your Unison scripts with a reference to this wrapper:
```unison
#!/usr/bin/env unison-script

-- Your Unison code here
main : '{base.IO, base.Exception} ()
main = do
  base.io.printLine "Hello from Unison!"
```

### 4. Using env -S (Linux/macOS)

On systems that support `env -S`, you can use multiple arguments in the shebang:

```unison
#!/usr/bin/env -S ucm run.file /dev/stdin main

-- Your Unison code here
main : '{base.IO, base.Exception} ()
main = do
  base.io.printLine "Hello from Unison!"
```

### 5. Using `ucm run` from Codebase

For code already in your codebase:

```bash
ucm run @myproject/mybranch:.path.to.main -- arg1 arg2
```

## Command Line Arguments

Unison programs access command line arguments using:
```unison
args = base.io.cli.getArgs ()
```

Arguments following `--` are passed to your program.

## Example: Complete Script

Here's a complete example of a Unison script that accepts arguments:

```unison
#!/usr/bin/env -S ucm run.file /dev/stdin main

greet : base.Text -> base.Text
greet name = base.Text.++ "Hello, " (base.Text.++ name "!")

main : '{base.IO, base.Exception} ()
main = do
  args = base.io.cli.getArgs ()
  case base.data.List.head args of
    None -> base.io.printLine "Usage: ./script.u <name>"
    Some name -> base.io.printLine (greet name)
```

Save it, make it executable, and run:
```bash
chmod +x script.u
./script.u Alice
```

## Important Notes

1. **Base Library Required**: Most scripts need the base library installed:
   ```
   ucm> lib.install @unison/base/releases/3.21.0
   ```

2. **Exit Flag**: Use `--exit` to prevent UCM from entering interactive mode:
   ```bash
   ucm --exit run.file script.u main
   ```

3. **Performance**: For frequently run scripts, consider compiling to `.uc` files for better performance.

4. **Path Considerations**: The shebang interpreter path must be absolute or use `/usr/bin/env`.

## Alternative: UCM Transcripts

For automated testing and scripting, consider using UCM transcripts (`.md` files) which can mix documentation, Unison code, and UCM commands.