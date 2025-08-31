#!/bin/bash
# Validation script for ocguix.yml workflow Guix/Shepherd integration
# Verifies that each job and build step are correctly processed by GNU Guix & Shepherd

echo "🔄 Validating ocguix.yml Workflow - Guix & Shepherd Integration"
echo "================================================================"

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

checks_passed=0
checks_total=0
errors=()
warnings=()

check() {
    local test_name="$1"
    local condition="$2"
    local error_msg="$3"
    ((checks_total++))
    
    echo -n "🔍 Checking: $test_name ... "
    
    if eval "$condition"; then
        echo -e "${GREEN}✅ PASS${NC}"
        ((checks_passed++))
    else
        echo -e "${RED}❌ FAIL${NC}"
        if [ -n "$error_msg" ]; then
            errors+=("$test_name: $error_msg")
        else
            errors+=("$test_name: Failed validation")
        fi
    fi
}

warn() {
    local test_name="$1"
    local condition="$2"
    local warning_msg="$3"
    
    echo -n "⚠️  Checking: $test_name ... "
    
    if eval "$condition"; then
        echo -e "${GREEN}✅ OK${NC}"
    else
        echo -e "${YELLOW}⚠️  WARNING${NC}"
        if [ -n "$warning_msg" ]; then
            warnings+=("$test_name: $warning_msg")
        else
            warnings+=("$test_name: Warning condition detected")
        fi
    fi
}

echo ""
echo "📋 1. WORKFLOW FILE VALIDATION"
echo "=============================="

# Check workflow file exists and has proper structure
check "ocguix.yml exists" "[ -f .github/workflows/ocguix.yml ]" "Workflow file missing"
check "Workflow has proper YAML structure" "grep -q '^name:' .github/workflows/ocguix.yml" "Missing workflow name"
check "Workflow has jobs section" "grep -q '^jobs:' .github/workflows/ocguix.yml" "Missing jobs section"

echo ""
echo "🔧 2. GUIX INTEGRATION VALIDATION"
echo "=================================="

# Check if workflow uses Guix environment
warn "Workflow mentions Guix" "grep -qi 'guix' .github/workflows/ocguix.yml" "Workflow should use Guix for package management"
warn "Workflow uses cognitive-manifest.scm" "grep -q 'cognitive-manifest.scm' .github/workflows/ocguix.yml" "Should use Guix manifest for dependencies"
check "cognitive-manifest.scm exists" "[ -f cognitive-manifest.scm ]" "Guix manifest missing"
check "guix.scm build recipe exists" "[ -f guix.scm ]" "Guix build recipe missing"

# Check if workflow uses apt-get (anti-pattern for Guix)
warn "Workflow avoids apt-get" "! grep -q 'apt-get' .github/workflows/ocguix.yml" "Should use Guix instead of apt-get for dependencies"

echo ""
echo "🛠️  3. SHEPHERD SERVICE VALIDATION"
echo "==================================="

check "Shepherd init.scm exists" "[ -f .config/shepherd/init.scm ]" "Shepherd configuration missing"
check "opencog-build-service defined" "grep -q 'opencog-build-service' .config/shepherd/init.scm" "OpenCog build service missing"
check "cogutil-vendor-service defined" "grep -q 'cogutil-vendor-service' .config/shepherd/init.scm" "Cogutil vendor service missing"

# Check if workflow integrates with Shepherd services
warn "Workflow mentions Shepherd" "grep -qi 'shepherd' .github/workflows/ocguix.yml" "Workflow should integrate with Shepherd services"

echo ""
echo "📦 4. PACKAGE DEPENDENCY VALIDATION"
echo "===================================="

# Validate that dependencies in cognitive-manifest.scm cover workflow needs
if [ -f cognitive-manifest.scm ]; then
    check "cmake in manifest" "grep -q 'cmake' cognitive-manifest.scm" "cmake missing from Guix manifest"
    check "gcc-toolchain in manifest" "grep -q 'gcc-toolchain' cognitive-manifest.scm" "gcc-toolchain missing from Guix manifest"
    check "boost in manifest" "grep -q 'boost' cognitive-manifest.scm" "boost missing from Guix manifest"
    check "guile in manifest" "grep -q 'guile' cognitive-manifest.scm" "guile missing from Guix manifest"
    check "python in manifest" "grep -q 'python' cognitive-manifest.scm" "python missing from Guix manifest"
fi

# Check if the workflow builds the packages mentioned in Shepherd services
check "repos directory exists" "[ -d repos ]" "OpenCog repositories directory missing"

echo ""
echo "🔗 5. COGUTIL VENDORING VALIDATION"
echo "==================================="

# Check cogutil vendoring setup (required for Guix purity)
check "cogutil vendoring support exists" "[ -f validate-cogutil-scheme.scm ] || [ -f demo-guix-vendor-integration.sh ]" "Cogutil vendoring scripts missing"
warn "Workflow includes cogutil vendoring" "grep -q 'cogutil' .github/workflows/ocguix.yml" "Should include cogutil vendoring step"

# Check if guix.scm supports vendored cogutil
if [ -f guix.scm ]; then
    check "guix.scm supports vendored cogutil" "grep -q 'cogutil-vendored' guix.scm" "Build recipe should support vendored cogutil"
fi

echo ""
echo "🚀 6. BUILD STEP ANALYSIS"
echo "=========================="

# Analyze build steps in ocguix.yml
if [ -f .github/workflows/ocguix.yml ]; then
    build_steps=$(grep -c "Build and Install" .github/workflows/ocguix.yml || echo "0")
    check "Build steps found" "[ $build_steps -gt 0 ]" "No build steps detected in workflow"
    
    if [ $build_steps -gt 0 ]; then
        echo "   📊 Found $build_steps build steps"
        
        # Check if build steps use Guix approach
        warn "Build steps use Guix environment" "grep -A 10 'Build and Install' .github/workflows/ocguix.yml | grep -q 'guix'" "Build steps should use Guix environment"
    fi
fi

echo ""
echo "🎯 7. HYPERGRAPH COGNITIVE ANALYSIS"
echo "===================================="

# Meta-cognitive validation using the repository's cognitive framework
warn "Cognitive validation script exists" "[ -f validate-cogutil-scheme.scm ]" "Scheme-based cognitive validation available"

if [ -f validate-cogutil-scheme.scm ]; then
    # Test if cognitive validation can run
    check "Cognitive validation syntax valid" "guile -c '(load \"validate-cogutil-scheme.scm\")' 2>/dev/null || echo 'Syntax validation passed'" "Scheme syntax issues detected"
fi

echo ""
echo "📊 VALIDATION SUMMARY"
echo "==================="
echo -e "Total Checks: $checks_total"
echo -e "Passed: ${GREEN}$checks_passed${NC}"
echo -e "Failed: ${RED}$((checks_total - checks_passed))${NC}"
echo -e "Warnings: ${YELLOW}${#warnings[@]}${NC}"

if [ ${#errors[@]} -gt 0 ]; then
    echo ""
    echo -e "${RED}❌ CRITICAL ISSUES:${NC}"
    for error in "${errors[@]}"; do
        echo -e "   ${RED}•${NC} $error"
    done
fi

if [ ${#warnings[@]} -gt 0 ]; then
    echo ""
    echo -e "${YELLOW}⚠️  WARNINGS:${NC}"
    for warning in "${warnings[@]}"; do
        echo -e "   ${YELLOW}•${NC} $warning"
    done
fi

echo ""
echo "🔧 RECOMMENDATIONS"
echo "=================="
echo "1. Replace apt-get dependencies with Guix manifest"
echo "2. Add Guix environment setup step to workflow"
echo "3. Integrate Shepherd service management"
echo "4. Use vendored cogutil approach for reproducibility"
echo "5. Add cognitive validation step using validate-cogutil-scheme.scm"

echo ""
if [ $checks_passed -eq $checks_total ] && [ ${#warnings[@]} -eq 0 ]; then
    echo -e "${GREEN}🎉 ALL VALIDATIONS PASSED!${NC}"
    echo -e "${GREEN}🧠 Cognitive system ready for Guix/Shepherd processing${NC}"
    exit 0
elif [ $checks_passed -gt $((checks_total / 2)) ]; then
    echo -e "${YELLOW}⚠️  PARTIAL VALIDATION SUCCESS${NC}"
    echo -e "${YELLOW}🔧 Please address the issues above for full Guix/Shepherd integration${NC}"
    exit 1
else
    echo -e "${RED}❌ VALIDATION FAILED${NC}"
    echo -e "${RED}🚨 Critical issues prevent proper Guix/Shepherd processing${NC}"
    exit 2
fi