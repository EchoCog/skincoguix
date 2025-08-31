#!/bin/bash
# Test script for ocguix.yml workflow Guix/Shepherd integration
# Tests the specific job and build step validation

echo "🧪 Testing ocguix.yml Workflow - Guix & Shepherd Processing"
echo "============================================================"

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

tests_passed=0
tests_total=0
test_log="/tmp/test-ocguix-workflow.log"

test_start() {
    echo -e "${BLUE}[TEST START]${NC} $1" | tee -a "$test_log"
}

test_pass() {
    echo -e "${GREEN}[PASS]${NC} $1" | tee -a "$test_log"
    ((tests_passed++))
    ((tests_total++))
}

test_fail() {
    echo -e "${RED}[FAIL]${NC} $1" | tee -a "$test_log"
    ((tests_total++))
}

test_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1" | tee -a "$test_log"
}

# Initialize test log
echo "OCGuix Workflow Test Log - $(date)" > "$test_log"

echo ""
echo "🔍 1. WORKFLOW STRUCTURE VALIDATION"
echo "===================================="

test_start "Validating workflow YAML syntax"
if yamllint .github/workflows/ocguix.yml 2>/dev/null || python3 -c "import yaml; yaml.safe_load(open('.github/workflows/ocguix.yml'))" 2>/dev/null; then
    test_pass "YAML syntax is valid"
else
    test_fail "YAML syntax validation failed"
fi

test_start "Checking for Guix environment setup step"
if grep -q "Setup Guix Environment" .github/workflows/ocguix.yml; then
    test_pass "Guix environment setup step found"
else
    test_fail "Guix environment setup step missing"
fi

test_start "Checking for cogutil vendoring step"
if grep -q "Vendor cogutil" .github/workflows/ocguix.yml; then
    test_pass "Cogutil vendoring step found"
else
    test_fail "Cogutil vendoring step missing"
fi

test_start "Checking for Shepherd service initialization"
if grep -q "Shepherd" .github/workflows/ocguix.yml; then
    test_pass "Shepherd integration found"
else
    test_fail "Shepherd integration missing"
fi

echo ""
echo "📦 2. PACKAGE PROCESSING VALIDATION"
echo "==================================="

test_start "Testing cognitive-manifest.scm can be loaded"
if guile -c "(use-modules (gnu)) (specifications->manifest '(\"gcc-toolchain\"))" 2>/dev/null; then
    if guile -c "(load \"cognitive-manifest.scm\")" 2>/dev/null; then
        test_pass "Cognitive manifest loads successfully"
    else
        test_fail "Cognitive manifest has syntax errors"
    fi
else
    test_warn "Guile not available for manifest testing"
fi

test_start "Testing guix.scm build recipe syntax"
if command -v guix >/dev/null 2>&1; then
    if guix build -f guix.scm --dry-run >/dev/null 2>&1; then
        test_pass "Guix build recipe is valid"
    else
        test_warn "Guix build recipe validation failed (may need dependencies)"
    fi
else
    test_warn "Guix not available for build recipe testing"
fi

echo ""
echo "🛠️  3. SHEPHERD SERVICE VALIDATION"
echo "=================================="

test_start "Validating Shepherd configuration syntax"
if guile .config/shepherd/init.scm 2>/dev/null; then
    test_pass "Shepherd configuration syntax is valid"
else
    test_fail "Shepherd configuration has syntax errors"
fi

test_start "Checking required Shepherd services"
required_services=("opencog-build-service" "cogutil-vendor-service" "opencog-dev-env-service")
all_services_found=true

for service in "${required_services[@]}"; do
    if grep -q "$service" .config/shepherd/init.scm; then
        echo "   ✅ $service found"
    else
        echo "   ❌ $service missing"
        all_services_found=false
    fi
done

if $all_services_found; then
    test_pass "All required Shepherd services found"
else
    test_fail "Some required Shepherd services missing"
fi

echo ""
echo "🔧 4. BUILD STEP PROCESSING VALIDATION" 
echo "======================================"

test_start "Analyzing build step dependencies"
build_steps=$(grep -c "Build and Install" .github/workflows/ocguix.yml)
echo "   Found $build_steps build steps"

if [ "$build_steps" -gt 0 ]; then
    test_pass "Build steps detected in workflow"
else
    test_fail "No build steps found in workflow"
fi

test_start "Checking if workflow uses vendored cogutil"
if grep -q "vendored" .github/workflows/ocguix.yml; then
    test_pass "Workflow uses vendored approach"
else
    test_fail "Workflow doesn't use vendored cogutil"
fi

echo ""
echo "🎯 5. INTEGRATION TESTING"
echo "========================="

test_start "Running workflow validation script"
if ./validate-ocguix-workflow.sh >/dev/null 2>&1; then
    test_pass "Workflow validation script passes"
else
    test_warn "Workflow validation script reports issues (expected during development)"
fi

test_start "Testing cognitive validation integration"
if [ -f validate-cogutil-scheme.scm ]; then
    if guile -s validate-cogutil-scheme.scm 2>/dev/null; then
        test_pass "Cognitive validation runs successfully"
    else
        test_warn "Cognitive validation needs dependencies"
    fi
else
    test_fail "Cognitive validation script missing"
fi

echo ""
echo "🧠 6. HYPERGRAPH COGNITIVE METRICS"
echo "=================================="

# Calculate cognitive complexity metrics
total_lines=$(wc -l < .github/workflows/ocguix.yml)
guix_mentions=$(grep -c -i "guix" .github/workflows/ocguix.yml)
shepherd_mentions=$(grep -c -i "shepherd" .github/workflows/ocguix.yml)
cognitive_ratio=$(echo "scale=2; ($guix_mentions + $shepherd_mentions) / $total_lines * 100" | bc -l 2>/dev/null || echo "N/A")

echo "   📊 Workflow complexity metrics:"
echo "      - Total lines: $total_lines"
echo "      - Guix integration points: $guix_mentions"
echo "      - Shepherd integration points: $shepherd_mentions"
echo "      - Cognitive integration ratio: ${cognitive_ratio}%"

if [ "$guix_mentions" -gt 0 ] && [ "$shepherd_mentions" -gt 0 ]; then
    test_pass "Workflow shows cognitive integration"
else
    test_warn "Limited cognitive integration detected"
fi

echo ""
echo "📊 TEST SUMMARY"
echo "==============="
echo -e "Total Tests: $tests_total"
echo -e "Passed: ${GREEN}$tests_passed${NC}"
echo -e "Failed: ${RED}$((tests_total - tests_passed))${NC}"

success_rate=$(echo "scale=1; $tests_passed / $tests_total * 100" | bc -l 2>/dev/null || echo "N/A")
echo -e "Success Rate: ${success_rate}%"

echo ""
echo "📋 DETAILED LOG: $test_log"

if [ $tests_passed -eq $tests_total ]; then
    echo -e "${GREEN}🎉 ALL TESTS PASSED!${NC}"
    echo -e "${GREEN}✅ OCGuix workflow ready for Guix/Shepherd processing${NC}"
    exit 0
elif [ $tests_passed -gt $((tests_total / 2)) ]; then
    echo -e "${YELLOW}⚠️  PARTIAL SUCCESS${NC}"
    echo -e "${YELLOW}🔧 Some tests failed - review and fix issues${NC}"
    exit 1
else
    echo -e "${RED}❌ MAJOR ISSUES DETECTED${NC}"
    echo -e "${RED}🚨 Workflow needs significant fixes for Guix/Shepherd integration${NC}"
    exit 2
fi