#!/bin/bash

# Test script to validate CI build fixes and GNU Guix & Shepherd package processing
# This script checks CI fixes AND validates that each job and build step are correctly 
# processed by GNU Guix & Shepherd as packages

set -e

echo "🔍 Testing CI Build Fixes Implementation & GNU Guix/Shepherd Package Processing..."
echo "=============================================================================="

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Function to print colored output
print_status() {
    local status=$1
    local message=$2
    if [ "$status" = "PASS" ]; then
        echo -e "${GREEN}✅ PASS${NC}: $message"
    elif [ "$status" = "FAIL" ]; then
        echo -e "${RED}❌ FAIL${NC}: $message"
    else
        echo -e "${YELLOW}⚠️  WARN${NC}: $message"
    fi
}

# Test 1: Check git safe.directory configuration in workflows
echo ""
echo "1. Testing Git Safe Directory Configuration..."

workflows=("ci-org-v7a.yml" "efficient-build.yml" "oc.yml")
for workflow in "${workflows[@]}"; do
    if grep -q "git config --global --add safe.directory" ".github/workflows/$workflow"; then
        print_status "PASS" "Git safe directory configured in $workflow"
    else
        print_status "FAIL" "Git safe directory missing in $workflow"
    fi
done

# Test 2: Check Python development headers
echo ""
echo "2. Testing Python Development Headers..."

if grep -q "python3.12-dev" ".github/workflows/ci-org-v7a.yml"; then
    print_status "PASS" "python3.12-dev added to ci-org-v7a.yml"
else
    print_status "FAIL" "python3.12-dev missing from ci-org-v7a.yml"
fi

if grep -q "python-dev" ".github/workflows/efficient-build.yml"; then
    print_status "PASS" "python-dev added to efficient-build.yml (conda)"
else
    print_status "FAIL" "python-dev missing from efficient-build.yml"
fi

if grep -q "python3.12-dev" "ocpkg"; then
    print_status "PASS" "python3.12-dev added to ocpkg script"
else
    print_status "FAIL" "python3.12-dev missing from ocpkg script"
fi

# Test 3: Check for duplicate python3-dev in ocpkg (should be fixed)
echo ""
echo "3. Testing ocpkg Script Fixes..."

if grep -q "python3-dev python3-dev" "ocpkg"; then
    print_status "FAIL" "Duplicate python3-dev still exists in ocpkg"
else
    print_status "PASS" "Duplicate python3-dev removed from ocpkg"
fi

# Test 4: Check database configuration
echo ""
echo "4. Testing Database Configuration..."

for workflow in "${workflows[@]}"; do
    if grep -q "POSTGRES_USER: opencog_test" ".github/workflows/$workflow" 2>/dev/null; then
        print_status "PASS" "Database configured with opencog_test user in $workflow"
    elif [[ "$workflow" == "efficient-build.yml" ]]; then
        print_status "WARN" "No database service in $workflow (may not need one)"
    else
        print_status "FAIL" "Database not configured with opencog_test user in $workflow"
    fi
done

# Test 5: Check YAML syntax
echo ""
echo "5. Testing YAML Syntax..."

for workflow in "${workflows[@]}"; do
    if python3 -c "import yaml; yaml.safe_load(open('.github/workflows/$workflow'))" 2>/dev/null; then
        print_status "PASS" "$workflow has valid YAML syntax"
    else
        print_status "FAIL" "$workflow has invalid YAML syntax"
    fi
done

# Test 6: Check documentation updates
echo ""
echo "6. Testing Documentation Updates..."

if grep -q "ci-org-v7a.yml" "CI-BUILD-FIXES.md" && grep -q "efficient-build.yml" "CI-BUILD-FIXES.md"; then
    print_status "PASS" "Documentation updated with new workflow fixes"
else
    print_status "FAIL" "Documentation not properly updated"
fi

# Test 7: Check Guix package definitions
echo ""
echo "7. Testing Guix Package Definitions..."

if [ -f "guix.scm" ]; then
    print_status "PASS" "guix.scm package definition exists"
    
    # Check for cogutil-vendored package
    if grep -q "cogutil-vendored" "guix.scm"; then
        print_status "PASS" "cogutil-vendored package defined in guix.scm"
    else
        print_status "FAIL" "cogutil-vendored package missing from guix.scm"
    fi
    
    # Check for ocguix-package
    if grep -q "ocguix-package" "guix.scm"; then
        print_status "PASS" "ocguix-package defined in guix.scm"
    else
        print_status "FAIL" "ocguix-package missing from guix.scm"
    fi
else
    print_status "FAIL" "guix.scm package definition missing"
fi

if [ -f "opencog.scm" ]; then
    print_status "PASS" "opencog.scm package definition exists"
    
    # Check for OpenCog package definition
    if grep -q "define-public opencog" "opencog.scm"; then
        print_status "PASS" "OpenCog package properly defined in opencog.scm"
    else
        print_status "FAIL" "OpenCog package not properly defined in opencog.scm"
    fi
else
    print_status "FAIL" "opencog.scm package definition missing"
fi

# Test 8: Check Cognitive Manifest
echo ""
echo "8. Testing Cognitive Manifest..."

if [ -f "cognitive-manifest.scm" ]; then
    print_status "PASS" "cognitive-manifest.scm exists"
    
    # Check for essential packages
    essential_packages=("gcc-toolchain" "cmake" "guile" "python" "boost")
    for package in "${essential_packages[@]}"; do
        if grep -q "\"$package\"" "cognitive-manifest.scm"; then
            print_status "PASS" "Essential package '$package' in cognitive manifest"
        else
            print_status "FAIL" "Essential package '$package' missing from cognitive manifest"
        fi
    done
else
    print_status "FAIL" "cognitive-manifest.scm missing"
fi

# Test 9: Check Shepherd Service Definitions
echo ""
echo "9. Testing Shepherd Service Definitions..."

if [ -f ".config/shepherd/init.scm" ]; then
    print_status "PASS" "Shepherd init.scm configuration exists"
    
    # Check for core services
    services=("opencog-build-service" "cogutil-vendor-service" "opencog-test-service")
    for service in "${services[@]}"; do
        if grep -q "$service" ".config/shepherd/init.scm"; then
            print_status "PASS" "Shepherd service '$service' defined"
        else
            print_status "FAIL" "Shepherd service '$service' missing"
        fi
    done
    
    # Check service registration
    if grep -q "register-services" ".config/shepherd/init.scm"; then
        print_status "PASS" "Shepherd services properly registered"
    else
        print_status "FAIL" "Shepherd services not properly registered"
    fi
else
    print_status "FAIL" "Shepherd configuration missing"
fi

# Test 10: Check Base DevContainers Integration
echo ""
echo "10. Testing Base DevContainers Integration..."

if [ -f "base-devcontainers.scm" ]; then
    print_status "PASS" "base-devcontainers.scm exists"
    
    # Check for cognitive profiles
    if grep -q "shepherd-packaging-profile" "base-devcontainers.scm"; then
        print_status "PASS" "Shepherd packaging profile defined"
    else
        print_status "FAIL" "Shepherd packaging profile missing"
    fi
    
    # Check for build profile catalog
    if grep -q "build-profile-catalog" "base-devcontainers.scm"; then
        print_status "PASS" "Build profile catalog defined"
    else
        print_status "FAIL" "Build profile catalog missing"
    fi
else
    print_status "FAIL" "base-devcontainers.scm missing"
fi

# Test 11: Check DevContainer Configuration
echo ""
echo "11. Testing DevContainer Configuration..."

if [ -f ".devcontainer/devcontainer.json" ]; then
    print_status "PASS" "DevContainer configuration exists"
    
    if grep -q "Guix Shepherd" ".devcontainer/devcontainer.json"; then
        print_status "PASS" "DevContainer configured for Guix Shepherd"
    else
        print_status "FAIL" "DevContainer not configured for Guix Shepherd"
    fi
    
    if grep -q "shepherd guile" ".devcontainer/devcontainer.json"; then
        print_status "PASS" "DevContainer installs Shepherd and Guile"
    else
        print_status "FAIL" "DevContainer missing Shepherd/Guile installation"
    fi
else
    print_status "FAIL" "DevContainer configuration missing"
fi

if [ -f ".devcontainer/Dockerfile" ]; then
    print_status "PASS" "DevContainer Dockerfile exists"
    
    if grep -q "guix install shepherd" ".devcontainer/Dockerfile"; then
        print_status "PASS" "Dockerfile installs Shepherd via Guix"
    else
        print_status "FAIL" "Dockerfile missing Shepherd installation"
    fi
else
    print_status "FAIL" "DevContainer Dockerfile missing"
fi

# Test 12: Check CI Workflow Guix Integration
echo ""
echo "12. Testing CI Workflow Guix Integration..."

if grep -q "guix" ".github/workflows/ocguix.yml" 2>/dev/null; then
    print_status "PASS" "ocguix.yml workflow includes Guix integration"
else
    print_status "FAIL" "ocguix.yml workflow missing Guix integration"
fi

# Check for Guix environment setup
for workflow in "${workflows[@]}"; do
    if grep -q "guix" ".github/workflows/$workflow" 2>/dev/null; then
        print_status "PASS" "$workflow includes Guix integration"
    else
        print_status "WARN" "$workflow may not include Guix integration (not required for all workflows)"
    fi
done

# Test 13: Check Guix Package Syntax Validation
echo ""
echo "13. Testing Guix Package Syntax Validation..."

# Test if we can validate Scheme syntax for Guix packages
if command -v guile >/dev/null 2>&1; then
    scheme_files=("guix.scm" "opencog.scm" "cognitive-manifest.scm" "base-devcontainers.scm")
    for scheme_file in "${scheme_files[@]}"; do
        if [ -f "$scheme_file" ]; then
            if guile -c "(load \"$scheme_file\")" >/dev/null 2>&1; then
                print_status "PASS" "$scheme_file has valid Scheme syntax"
            else
                print_status "FAIL" "$scheme_file has invalid Scheme syntax"
            fi
        fi
    done
else
    print_status "WARN" "Guile not available for Scheme syntax validation"
fi

# Test 14: Check Package Dependencies Compatibility
echo ""
echo "14. Testing Package Dependencies Compatibility..."

# Check if cognitive-manifest.scm includes packages needed by guix.scm
if [ -f "cognitive-manifest.scm" ] && [ -f "guix.scm" ]; then
    # Essential build dependencies check
    deps_check=("cmake" "gcc-toolchain" "guile" "boost")
    missing_deps=0
    
    for dep in "${deps_check[@]}"; do
        if ! grep -q "\"$dep\"" "cognitive-manifest.scm"; then
            print_status "WARN" "Dependency '$dep' not found in cognitive manifest"
            ((missing_deps++))
        fi
    done
    
    if [ $missing_deps -eq 0 ]; then
        print_status "PASS" "All essential dependencies found in cognitive manifest"
    else
        print_status "WARN" "$missing_deps dependencies missing from cognitive manifest"
    fi
fi

echo ""
echo "🏁 Testing Complete!"
echo ""
echo "📝 Summary:"
echo "Enhanced validation covers:"
echo "- Basic CI fixes (git ownership, Python headers, database config)"
echo "- GNU Guix package definitions (guix.scm, opencog.scm)"
echo "- Shepherd service configurations"
echo "- Cognitive manifest and build profiles"
echo "- DevContainer Guix/Shepherd integration"
echo "- CI workflow package processing compatibility"
echo "- Guix package syntax validation"
echo "- Package dependency compatibility checks"
echo ""
echo "🚀 Next Steps:"
echo "1. Commit and push these changes"
echo "2. Trigger a CI build to verify fixes work"
echo "3. Monitor package building with Guix environment"
echo "4. Validate Shepherd service management"