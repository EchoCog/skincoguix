#!/bin/bash
# Simple validation test for Guix Shepherd devcontainer setup

echo "=== Guix Shepherd Devcontainer Setup Validation ==="

checks_passed=0
checks_total=0

check() {
    local test_name="$1"
    local condition="$2"
    ((checks_total++))
    
    if eval "$condition"; then
        echo "✅ $test_name"
        ((checks_passed++))
    else
        echo "❌ $test_name"
    fi
}

# Check devcontainer files
check "Dockerfile exists" "[ -f .devcontainer/Dockerfile ]"
check "devcontainer.json exists" "[ -f .devcontainer/devcontainer.json ]"
check "devcontainer.json mentions Guix Shepherd" "grep -q 'Guix Shepherd' .devcontainer/devcontainer.json"

# Check OpenCog package definition
check "opencog.scm exists" "[ -f opencog.scm ]"
check "OpenCog package defined" "grep -q 'define-public opencog' opencog.scm"
check "Git checkout configured" "grep -q 'git-checkout' opencog.scm"

# Check Shepherd configuration
check "Shepherd init.scm exists" "[ -f .config/shepherd/init.scm ]"
check "OpenCog build service defined" "grep -q 'opencog-build-service' .config/shepherd/init.scm"
check "Cogutil vendor service defined" "grep -q 'cogutil-vendor-service' .config/shepherd/init.scm"

# Check base-devcontainers.scm integration
check "Shepherd packaging profile added" "grep -q 'shepherd-packaging-profile' base-devcontainers.scm"

echo ""
echo "=== Summary ==="
echo "Passed: $checks_passed/$checks_total"

if [ $checks_passed -eq $checks_total ]; then
    echo "🎉 All checks passed! Guix Shepherd devcontainer setup is complete."
    exit 0
else
    echo "⚠️  Some checks failed. Please review the setup."
    exit 1
fi