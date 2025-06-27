# Unison Version Management Guide

## Overview

Unison uses structural version management, which differs from traditional text-based version control.

## Unison's Unique Version Management

### 1. Content-Addressed Storage
- Code is stored as syntax trees (AST)
- Each definition is identified by a content hash
- Functions with identical content have the same hash

### 2. Structural Change Tracking
```unison
-- Version 1
myFunction : Nat -> Nat
myFunction n = n + 1

-- Version 2 (Breaking change)
myFunction : Int -> Int
myFunction n = n + 1
```
When types change, they are treated as different definitions.

## Release Version Management

### Semantic Versioning

Unison uses Semantic Versioning (SemVer):
- **Major (X.0.0)**: Breaking changes
- **Minor (0.X.0)**: Backward-compatible new features
- **Patch (0.0.X)**: Bug fixes

### Creating Releases

#### 1. Create Draft Release
```ucm
myproject/main> release.draft 1.0.0
```

This creates a `/releases/drafts/1.0.0` branch.

#### 2. Add Release Notes
```unison
ReleaseNotes : Doc
ReleaseNotes = {{
# v1.0.0

## New Features
- Initial release
- Basic MCP type definitions
- Request/response builders

## Breaking Changes
- None

## Bug Fixes
- None
}}
```

#### 3. Publish Release
```ucm
myproject/releases/drafts/1.0.0> push
```

Then click "Publish" on Unison Share.

## Managing Compatibility Between Versions

### 1. Avoid Type Changes
```unison
-- Good: Add a new function
myFunction.v2 : Int -> Int -> Int
myFunction.v2 x y = x + y

-- Bad: Change existing type
myFunction : Int -> Int  -- Breaking change!
```

### 2. Deprecation Management
```unison
myOldFunction.deprecated : Doc
myOldFunction.deprecated = {{
  ⚠️ This function is deprecated.
  Please use `myNewFunction` instead.
}}

myOldFunction : Nat -> Nat
myOldFunction n = myNewFunction n
```

### 3. Provide Migration Guides
```unison
MigrationGuide : Doc
MigrationGuide = {{
# Migrating from v1.0.0 to v2.0.0

## Changes
1. `oldFunction` renamed to `newFunction`
2. Added new field to `Config` type

## Migration Steps
```unison
-- Old
result = oldFunction 42

-- New
result = newFunction 42
```
}}
```

## Branch Strategy

### Recommended Structure
```
myproject/
├── main            # Latest development
├── releases/       # Released versions
│   ├── 1.0.0
│   ├── 1.1.0
│   └── 2.0.0
└── feature/        # Feature development
    └── new-feature
```

### Example Workflow
```ucm
-- Feature development
myproject/main> branch.create feature/new-api
myproject/feature/new-api> -- Development work

-- Merge to main
myproject/main> merge feature/new-api

-- Prepare release
myproject/main> release.draft 1.1.0
myproject/releases/drafts/1.1.0> push
```

## Dependency Version Management

### 1. Specific Version Dependencies
```ucm
.> lib.install @unison/base/releases/3.21.0
```

### 2. Updating Dependencies
```ucm
-- Check current dependencies
.> ls lib

-- Install new version
.> lib.install @unison/base/releases/3.22.0

-- Remove old version (optional)
.> delete.namespace lib.unison_base_3_21_0
```

### 3. Pinning Dependencies
Document in project README:
```unison
README : Doc
README = {{
## Dependencies
- @unison/base/releases/3.21.0
- @unison/http/releases/1.0.0
}}
```

## Version Management Best Practices

### 1. Maintain Changelogs
Provide detailed changelogs for each release:
```unison
Changelog : Doc
Changelog = {{
# Changelog

## [1.2.0] - 2024-06-27
### Added
- New utility functions

### Changed
- Performance improvements

### Fixed
- Bug fix #123
}}
```

### 2. Maintain Tests
```unison
-- Version-specific tests
test> v1.compatibility.test = 
  check (oldAPI.function 5 == newAPI.function 5)
```

### 3. Version Documentation
```unison
-- Version-specific docs
DocsV1 : Doc
DocsV1 = {{ Documentation for version 1.x }}

DocsV2 : Doc  
DocsV2 = {{ Documentation for version 2.x }}

-- Current documentation
Docs = DocsV2
```

## Example: MCP Library Version Management

```ucm
-- Initial release
mcp/main> release.draft 0.1.0
mcp/releases/drafts/0.1.0> push

-- Feature addition (minor version)
mcp/main> -- Add new features
mcp/main> release.draft 0.2.0

-- Bug fix (patch version)
mcp/releases/0.2.0> -- Fix bugs
mcp/releases/0.2.0> release.draft 0.2.1

-- Breaking changes (major version)
mcp/main> -- Change API
mcp/main> release.draft 1.0.0
```

## Summary

Unison's version management is:
1. **Structural**: Based on code semantics
2. **Persistent**: Past versions are always available
3. **Explicit**: Breaking changes are treated as new definitions

This minimizes dependency issues while allowing safe library evolution.