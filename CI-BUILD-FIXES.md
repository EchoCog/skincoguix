# CI Build Fixes Documentation

## Overview

This document outlines the fixes implemented to resolve CI build failures in the OzCog/ocguix project. The fixes address multiple issues that were causing workflow builds to fail.

## Issues Identified and Fixed

### 1. Git Dubious Ownership Error

**Problem**: 
```
fatal: detected dubious ownership in repository at '/__w/ocguix/ocguix'
```

**Root Cause**: 
Git security measures prevent operations on repositories owned by different users in CI environments.

**Solution**: 
Added git safe.directory configuration in the workflow:

```yaml
- name: Set Git Safe Directory
  run: |
    git config --global --add safe.directory ${{ github.workspace }}
    git config --global --add safe.directory /__w/ocguix/ocguix
```

**Files Modified**: 
- `.github/workflows/oc.yml`
- `.github/workflows/ci-org-v7a.yml`
- `.github/workflows/efficient-build.yml`

### 2. Valgrind Not Found Error

**Problem**: 
```
Could NOT find VALGRIND (missing: VALGRIND_INCLUDE_DIR VALGRIND_PROGRAM)
```

**Root Cause**: 
Valgrind was not installed in the CI environment, but CMake was trying to find it for thread debugging.

**Solution**: 
Added valgrind installation to the build dependencies:

```yaml
- name: Install Build Dependencies
  run: |
    sudo apt-get update
    sudo apt-get install -y ccache pkg-config cmake build-essential git valgrind
```

**Files Modified**: 
- `.github/workflows/oc.yml`

### 3. C++ Compilation Error: 'ostream_container' Not Declared

**Problem**: 
```
'ostream_container' was not declared in this scope
Location: /repos/moses/moses/comboreduct/table/table.h:118
```

**Root Cause**: 
The `ostream_container` function is defined in `opencog/util/iostreamContainer.h` but was not included in `table.h`.

**Solution**: 
Added the missing include statement:

```cpp
#include <opencog/util/iostreamContainer.h>
```

**Files Modified**: 
- `repos/moses/moses/comboreduct/table/table.h`

### 4. Database Role Issues

**Problem**: 
```
FATAL: role "root" does not exist
```

**Root Cause**: 
Database initialization was trying to use a 'root' role that doesn't exist in the PostgreSQL container.

**Solution**: 
The workflow already uses the correct database configuration:
- `POSTGRES_USER: opencog_test`
- `POSTGRES_PASSWORD: cheese`
- `POSTGRES_DB: atomspace_db`

This ensures the database uses the `opencog_test` user instead of `root`.

### 5. Missing Python Development Headers

**Problem**: 
```
Cython compilation failures due to missing Python development headers
```

**Root Cause**: 
Python development headers (python3-dev and python3.12-dev) were not installed before Cython build, causing compilation errors in Python extension modules.

**Solution**: 
Added python3.12-dev to the dependency installation alongside python3-dev:

```yaml
- name: Install Build Dependencies
  run: |
    sudo apt-get install -y python3 python3-dev python3.12-dev python3-pip python3-venv python3-nose cython3
```

For conda-based workflows, added python-dev package:

```yaml
conda install -y -c conda-forge \
  cmake \
  boost-cpp \
  python \
  python-dev \
  cython \
  pkg-config
```

**Files Modified**: 
- `.github/workflows/ci-org-v7a.yml`
- `.github/workflows/efficient-build.yml`
- `ocpkg` (installation script)

## Implementation Details

### Workflow Changes

The workflow files have been updated with comprehensive fixes:

**`.github/workflows/oc.yml`** (already had fixes):
1. **Git Safe Directory Configuration**: Added immediately after checkout to prevent ownership issues
2. **Valgrind Installation**: Added to the build dependencies step
3. **Proper Error Handling**: Maintained existing error handling patterns

**`.github/workflows/ci-org-v7a.yml`** (newly fixed):
1. **Git Safe Directory Configuration**: Added after checkout step
2. **Python Development Headers**: Added python3.12-dev to dependency installation
3. **Database Configuration**: Already properly configured with opencog_test user

**`.github/workflows/efficient-build.yml`** (newly fixed):
1. **Git Safe Directory Configuration**: Added to all checkout jobs (setup-dependencies, build-foundation, build-failing-components, integration-test)
2. **Python Development Headers**: Added python-dev to conda installation
3. **Conda-based Dependencies**: Using conda for faster, more reliable package management

### Code Changes

The `table.h` file was updated to include the missing header:

```cpp
// Before
#include <opencog/util/algorithm.h>
#include <opencog/util/Counter.h>
#include <opencog/util/dorepeat.h>
#include <opencog/util/exceptions.h>
#include <opencog/util/KLD.h>

// After
#include <opencog/util/algorithm.h>
#include <opencog/util/Counter.h>
#include <opencog/util/dorepeat.h>
#include <opencog/util/exceptions.h>
#include <opencog/util/KLD.h>
#include <opencog/util/iostreamContainer.h>
```

## Testing and Validation

### Pre-Fix Issues
- Git operations failing due to ownership
- CMake unable to find Valgrind
- Compilation errors in moses component
- Database connection failures

### Post-Fix Expected Behavior
- Git operations proceed normally
- Valgrind found and configured properly
- All components compile successfully
- Database connections work with `opencog_test` user

## Monitoring and Prevention

### Issue Template
Created `.github/ISSUE_TEMPLATE/ci-build-failure.md` to standardize CI failure reporting.

### Common Solutions Reference
The issue template includes quick reference solutions for:
- Git ownership issues
- Missing dependencies
- Compilation errors
- Database configuration

## Future Improvements

### Recommended Enhancements
1. **Dependency Validation**: Add pre-build checks for required tools
2. **Better Error Messages**: Improve CMake error reporting
3. **Build Caching**: Implement ccache for faster rebuilds
4. **Parallel Testing**: Enable parallel test execution where possible

### Monitoring
- Monitor workflow success rates
- Track build times
- Document any new failure patterns
- Update this documentation as needed

## Troubleshooting Guide

### If Builds Still Fail

1. **Check Git Configuration**:
   ```bash
   git config --global --list | grep safe.directory
   ```

2. **Verify Valgrind Installation**:
   ```bash
   which valgrind
   valgrind --version
   ```

3. **Check Include Paths**:
   ```bash
   find /usr/local/include -name "iostreamContainer.h"
   ```

4. **Database Connection Test**:
   ```bash
   psql -h localhost -U opencog_test -d atomspace_db -c "SELECT 1;"
   ```

### Common Commands

```bash
# Fix git ownership
git config --global --add safe.directory /path/to/repo

# Install valgrind
sudo apt-get update && sudo apt-get install -y valgrind

# Check CMake configuration
cmake --find-package -DNAME=VALGRIND -DCOMPILER_ID=GNU -DLANGUAGE=CXX -DMODE=EXIST

# Test database connection
psql -h localhost -U opencog_test -d atomspace_db
```

## Conclusion

These fixes address the core issues causing CI build failures across multiple workflows:
- Git ownership problems resolved with safe.directory configuration in all affected workflows
- Python development headers ensured with python3-dev and python3.12-dev installation
- Valgrind dependency issue fixed with proper installation
- Compilation error resolved with missing include
- Database configuration already properly set up with opencog_test user

**Workflows Fixed**:
- `.github/workflows/oc.yml` (already had fixes)
- `.github/workflows/ci-org-v7a.yml` (newly fixed)
- `.github/workflows/efficient-build.yml` (newly fixed - includes Moses build)

The workflows should now build successfully across all components, including the Moses component that was failing at 98% completion. Monitor the builds and update this documentation if new issues arise.

## Enhanced Validation with test-ci-fixes.sh

The `test-ci-fixes.sh` script has been enhanced to provide comprehensive validation of both CI build fixes AND GNU Guix & Shepherd package processing compatibility. The enhanced validation covers:

### Core CI Fixes
- Git safe directory configuration across all workflows
- Python development headers installation
- Database configuration with opencog_test user
- YAML syntax validation
- Documentation updates

### GNU Guix & Shepherd Package Validation
- **Guix Package Definitions**: Validates `guix.scm` and `opencog.scm` package definitions
- **Cognitive Manifest**: Checks `cognitive-manifest.scm` for essential dependencies
- **Shepherd Services**: Validates `.config/shepherd/init.scm` service definitions
- **DevContainer Integration**: Checks Guix/Shepherd devcontainer configuration
- **CI Workflow Compatibility**: Ensures workflows are compatible with Guix environments
- **Package Syntax**: Validates Scheme syntax of package definitions (when Guile available)
- **Dependency Compatibility**: Verifies package dependencies are properly configured

### Usage
```bash
# Run comprehensive validation
./test-ci-fixes.sh

# The script will validate:
# ✅ All CI build fixes are properly implemented
# ✅ GNU Guix package definitions are valid
# ✅ Shepherd services are properly configured
# ✅ DevContainer setup supports Guix/Shepherd
# ✅ CI workflows are compatible with Guix package management
```

This enhanced validation ensures that each job and build step are correctly processed by GNU Guix & Shepherd as packages, addressing the requirements in issue #35.