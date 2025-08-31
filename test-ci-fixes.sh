#!/bin/bash

# Test script to validate CI build fixes implementation
# This script checks that the fixes for git ownership, Python headers, and Postgres role are properly implemented

set -e

echo "🔍 Testing CI Build Fixes Implementation..."
echo "=========================================="

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

echo ""
echo "🏁 Testing Complete!"
echo ""
echo "📝 Summary:"
echo "These fixes address the issues mentioned in the GitHub issue:"
echo "- Git ownership errors (git config --global --add safe.directory)"
echo "- Missing Python headers (python3-dev and python3.12-dev)"
echo "- Postgres role configuration (already using opencog_test user)"
echo ""
echo "🚀 Next Steps:"
echo "1. Commit and push these changes"
echo "2. Trigger a CI build to verify fixes work"
echo "3. Monitor the Moses job for successful completion at 98%"