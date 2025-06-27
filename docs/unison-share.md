# Unison Share User Guide

Unison Share is the official platform for sharing and distributing Unison code.

## Table of Contents
- [Overview](#overview)
- [Account Creation and Authentication](#account-creation-and-authentication)
- [Publishing Libraries](#publishing-libraries)
- [Using Libraries](#using-libraries)
- [Project Management](#project-management)
- [Best Practices](#best-practices)

## Overview

Key features of Unison Share:
- Permanent code storage
- Semantic versioning
- Automatic dependency management
- Web-based code browsing

## Account Creation and Authentication

### 1. Create an Account
Create an account at [https://share.unison-lang.org/](https://share.unison-lang.org/).

### 2. Authentication in UCM
```ucm
.> auth.login
```
This opens your browser and starts the authentication process.

### 3. Check Authentication Status
```ucm
.> auth.status
```

## Publishing Libraries

### 1. Create a Project
```ucm
.> project.create myproject
```

### 2. Switch to the Project
```ucm
.> switch myproject/main
```

### 3. Add Your Code
```unison
-- mylib.u
myFunction : Nat -> Nat
myFunction n = n + 1
```

```ucm
myproject/main> load mylib.u
myproject/main> update
```

### 4. Add README (Recommended)
```unison
README : Doc
README = {{
# My Library

Description of this library

## Installation
```ucm
lib.install @username/myproject
```

## Usage
```unison
use myproject.myFunction
result = myFunction 5
```
}}
```

### 5. Add ReleaseNotes (Recommended)
```unison
ReleaseNotes : Doc
ReleaseNotes = {{
# v0.1.0

Initial release

## New Features
- Added myFunction
}}
```

### 6. Push to Unison Share
```ucm
myproject/main> push
```

This publishes your project as `@username/myproject`.

### 7. Configure Project (On Website)
1. Visit the displayed URL
2. Click "Settings"
3. Set to "Public"
4. Add a project description

## Using Libraries

### 1. Install a Library
```ucm
.> lib.install @username/libraryname
```

### 2. Install a Specific Version
```ucm
.> lib.install @username/libraryname/releases/1.0.0
```

### 3. Check Installed Libraries
```ucm
.> ls lib
```

### 4. Use the Library
```unison
use lib.username_libraryname_1_0_0

-- Use library functions
result = lib.username_libraryname_1_0_0.someFunction 42
```

## Project Management

### Creating Releases

#### Method 1: Via Web Interface
1. Push the `main` branch
2. Go to the "releases" tab on Unison Share
3. Click "Cut a release"
4. Choose version (major/minor/patch)
5. Click "Publish"

#### Method 2: Via UCM
```ucm
myproject/main> release.draft 1.0.0
myproject/releases/drafts/1.0.0> push
```
Then finalize the release on the web.

### Branch Management
```ucm
-- Create a new branch
myproject/main> branch.create feature/new-feature

-- Switch branches
myproject/main> switch myproject/feature/new-feature

-- Merge a branch
myproject/main> merge myproject/feature/new-feature
```

### Deleting a Project
You can delete projects from the "Settings" page on the web interface.

## Best Practices

### 1. Naming Conventions
- Use lowercase and hyphens for project names (e.g., `my-cool-library`)
- Organize namespaces hierarchically

### 2. Documentation
- Always include a README
- Add ReleaseNotes for each release
- Add doc comments to functions

```unison
myFunction.doc : Doc
myFunction.doc = {{
  Adds 1 to a number.
  
  ## Example
  ```
  myFunction 5 -- Result: 6
  ```
}}
```

### 3. Versioning
- Follow semantic versioning
- Major version for breaking changes
- Minor version for new features
- Patch version for bug fixes

### 4. Dependencies
- Place dependencies in the `lib` namespace
- Keep dependencies minimal

### 5. Testing
```unison
test> myFunction.tests.addition = 
  check (myFunction 5 == 6)
```

## Troubleshooting

### Authentication Errors
```ucm
.> auth.logout
.> auth.login
```

### Push Failures
- Check network connection
- Verify authentication status
- Check for project name conflicts

### Library Not Found
- Verify correct naming
- Ensure project is set to Public
- Make sure to use `@` prefix with `lib.install`

## Related Links

- [Unison Share](https://share.unison-lang.org/)
- [Official Unison Documentation](https://www.unison-lang.org/docs/)
- [Popular Libraries](https://share.unison-lang.org/catalog/namespaces)

## Example: Publishing the MCP Library

```ucm
-- 1. Create project
.> project.create mcp
.> switch mcp/main

-- 2. Add code
mcp/main> load scratch.u
mcp/main> update

-- 3. Publish
mcp/main> push

-- 4. Usage
.> lib.install @mizchi/mcp
```

This publishes the library as `@mizchi/mcp` for anyone to use.