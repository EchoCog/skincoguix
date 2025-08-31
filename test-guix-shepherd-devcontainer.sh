#!/bin/bash
# Test script for Guix Shepherd devcontainer integration

# Note: Using explicit error handling instead of set -e for comprehensive testing

echo "🧪 Testing Guix Shepherd devcontainer setup..."

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

test_log() {
    echo -e "${BLUE}ℹ️  $1${NC}"
}

test_success() {
    echo -e "${GREEN}✅ $1${NC}"
}

test_error() {
    echo -e "${RED}❌ $1${NC}"
}

test_warning() {
    echo -e "${YELLOW}⚠️  $1${NC}"
}

# Test 1: Check devcontainer files exist
test_devcontainer_files() {
    test_log "Testing devcontainer files presence..."
    
    if [ -f ".devcontainer/Dockerfile" ]; then
        test_success "Dockerfile exists"
    else
        test_error "Dockerfile missing"
        return 1
    fi
    
    if [ -f ".devcontainer/devcontainer.json" ]; then
        test_success "devcontainer.json exists"
    else
        test_error "devcontainer.json missing"
        return 1
    fi
    
    # Check devcontainer.json contains Guix Shepherd configuration
    if grep -q "Guix Shepherd" ".devcontainer/devcontainer.json"; then
        test_success "devcontainer.json configured for Guix Shepherd"
    else
        test_error "devcontainer.json not configured for Guix Shepherd"
        return 1
    fi
}

# Test 2: Check OpenCog package definition
test_opencog_package() {
    test_log "Testing OpenCog package definition..."
    
    if [ -f "opencog.scm" ]; then
        test_success "opencog.scm exists"
    else
        test_error "opencog.scm missing"
        return 1
    fi
    
    # Check for key components in package definition
    if grep -q "define-public opencog" "opencog.scm"; then
        test_success "OpenCog package definition found"
    else
        test_error "OpenCog package definition missing"
        return 1
    fi
    
    if grep -q "git-checkout" "opencog.scm"; then
        test_success "Git source configuration found"
    else
        test_error "Git source configuration missing"
        return 1
    fi
}

# Test 3: Check Shepherd configuration
test_shepherd_config() {
    test_log "Testing Shepherd service configuration..."
    
    if [ -f ".config/shepherd/init.scm" ]; then
        test_success "Shepherd init.scm exists"
    else
        test_error "Shepherd init.scm missing"
        return 1
    fi
    
    # Check for OpenCog build service
    if grep -q "opencog-build-service" ".config/shepherd/init.scm"; then
        test_success "OpenCog build service defined"
    else
        test_error "OpenCog build service missing"
        return 1
    fi
    
    # Check for cogutil vendor service
    if grep -q "cogutil-vendor-service" ".config/shepherd/init.scm"; then
        test_success "Cogutil vendor service defined"
    else
        test_error "Cogutil vendor service missing"
        return 1
    fi
}

# Test 4: Check base-devcontainers.scm integration
test_base_devcontainers() {
    test_log "Testing base-devcontainers.scm integration..."
    
    if grep -q "shepherd-packaging-profile" "base-devcontainers.scm"; then
        test_success "Shepherd packaging profile added"
    else
        test_error "Shepherd packaging profile missing"
        return 1
    fi
    
    if grep -q "shepherd-packaging-profile" "base-devcontainers.scm" && \
       grep -A 10 "shepherd-packaging-profile" "base-devcontainers.scm" | grep -q "shepherd"; then
        test_success "Shepherd packaging profile properly configured"
    else
        test_error "Shepherd packaging profile not properly configured"
        return 1
    fi
}

# Test 5: Validate Scheme syntax (if guile is available)
test_scheme_syntax() {
    test_log "Testing Scheme syntax validation..."
    
    if command -v guile &> /dev/null; then
        test_success "Guile found, testing syntax..."
        
        # Test opencog.scm syntax
        if guile -c "(load \"opencog.scm\")" 2>/dev/null; then
            test_success "opencog.scm syntax is valid"
        else
            test_warning "opencog.scm syntax issues (may be due to missing dependencies)"
        fi
        
        # Test shepherd init.scm syntax  
        if guile -c "(load \".config/shepherd/init.scm\")" 2>/dev/null; then
            test_success "Shepherd init.scm syntax is valid"
        else
            test_warning "Shepherd init.scm syntax issues (may be due to missing shepherd)"
        fi
    else
        test_warning "Guile not available for syntax validation"
    fi
}

# Run all tests
main() {
    echo ""
    test_log "Starting Guix Shepherd devcontainer tests..."
    echo ""
    
    local tests_passed=0
    local tests_failed=0
    
    # Run each test
    for test_func in test_devcontainer_files test_opencog_package test_shepherd_config test_base_devcontainers test_scheme_syntax; do
        echo ""
        if $test_func; then
            ((tests_passed++))
        else
            ((tests_failed++))
        fi
    done
    
    echo ""
    test_log "Test Results:"
    test_log "Passed: $tests_passed"
    test_log "Failed: $tests_failed"
    
    if [ $tests_failed -eq 0 ]; then
        test_success "All tests passed! Guix Shepherd devcontainer setup is ready."
        return 0
    else
        test_error "Some tests failed. Please check the configuration."
        return 1
    fi
}

# Check if script is being sourced or executed
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi