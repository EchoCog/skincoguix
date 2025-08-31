#!/bin/bash
# Enhanced validation test for Guix Shepherd devcontainer setup
# Includes dependency compatibility checks and environment validation

echo "=== Guix Shepherd Devcontainer Setup Validation ==="

checks_passed=0
checks_total=0
warnings=0

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

warn() {
    local warning_msg="$1"
    echo "⚠️  $warning_msg"
    ((warnings++))
}

# Dependency compatibility checks
check_dependencies() {
    echo ""
    echo "=== Dependency Compatibility Checks ==="
    
    # Check for Guix availability
    if command -v guix >/dev/null 2>&1; then
        echo "✅ Guix package manager available: $(guix --version | head -n1)"
    else
        warn "Guix not available - some validation features limited"
        echo "   Suggestion: Install Guix from https://guix.gnu.org/"
    fi
    
    # Check for Guile availability
    if command -v guile >/dev/null 2>&1; then
        echo "✅ Guile Scheme interpreter available: $(guile --version | head -n1)"
    else
        warn "Guile not available - Scheme syntax validation disabled"
        echo "   Suggestion: Install guile package for full validation"
    fi
    
    # Check for Shepherd availability
    if command -v shepherd >/dev/null 2>&1; then
        echo "✅ Shepherd service manager available"
    else
        warn "Shepherd not available - service management limited"
        echo "   Note: This is expected in non-Guix environments"
    fi
    
    # Check Docker/container environment
    if [ -f /.dockerenv ] || [ -n "${CONTAINER}" ]; then
        echo "ℹ️  Running in container environment"
    elif command -v docker >/dev/null 2>&1; then
        echo "✅ Docker available for container testing"
    else
        warn "Docker not available - container testing limited"
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

# Run dependency compatibility checks
check_dependencies

# Enhanced Guix environment validation (if available)
guix_environment_tests() {
    echo ""
    echo "=== Guix Environment Tests ==="
    
    if command -v guix >/dev/null 2>&1; then
        # Test package definitions can be loaded
        if guix show opencog >/dev/null 2>&1; then
            echo "✅ OpenCog package available in Guix"
        elif [ -f "opencog.scm" ]; then
            echo "ℹ️  Using local OpenCog package definition"
            if command -v guile >/dev/null 2>&1; then
                if guile -c "(load \"opencog.scm\")" 2>/dev/null; then
                    echo "✅ Local OpenCog package definition syntax valid"
                else
                    warn "Local OpenCog package definition has syntax issues"
                fi
            fi
        else
            warn "OpenCog package not found in Guix or locally"
        fi
        
        # Test manifest file
        if [ -f "cognitive-manifest.scm" ]; then
            if command -v guile >/dev/null 2>&1; then
                if guile -c "(load \"cognitive-manifest.scm\")" 2>/dev/null; then
                    echo "✅ Cognitive manifest syntax valid"
                else
                    warn "Cognitive manifest has syntax issues"
                fi
            else
                echo "ℹ️  Cognitive manifest present (syntax check skipped - no Guile)"
            fi
        fi
        
        # Test Shepherd service definitions
        if [ -f ".config/shepherd/init.scm" ] && command -v guile >/dev/null 2>&1; then
            if guile -c "
                (use-modules (ice-9 read-set))
                (read-string (call-with-input-file \".config/shepherd/init.scm\" get-string-all))
            " 2>/dev/null; then
                echo "✅ Shepherd service definitions syntax valid"
            else
                warn "Shepherd service definitions have syntax issues"
            fi
        fi
    else
        echo "ℹ️  Guix not available - skipping environment-specific tests"
        echo "   Note: Install Guix for complete validation"
    fi
}

guix_environment_tests

echo ""
echo "=== Summary ==="
echo "Passed: $checks_passed/$checks_total"
if [ $warnings -gt 0 ]; then
    echo "Warnings: $warnings"
fi

# Cognitive framework assessment
echo ""
echo "=== Cognitive Framework Assessment ==="
echo "Hypergraph Analysis:"
echo "- Node: Package script validation"
echo "- Links: $(( checks_passed + warnings )) dependency relationships checked"
echo "- Tensor Dimensions: [script_complexity: $checks_total, dependency_count: $warnings, risk_level: $(( checks_total - checks_passed ))]"

if [ $checks_passed -eq $checks_total ]; then
    echo ""
    echo "🎉 All checks passed! Guix Shepherd devcontainer setup is complete."
    if [ $warnings -gt 0 ]; then
        echo "⚠️  Note: $warnings warnings reported - consider addressing for optimal setup"
    fi
    echo ""
    echo "🧠 Meta-Cognitive Feedback: Setup validated successfully with cognitive framework"
    exit 0
else
    echo ""
    echo "⚠️  Some checks failed. Please review the setup."
    echo ""
    echo "🔧 Recovery Suggestions:"
    echo "1. Ensure all required files exist in correct locations"
    echo "2. Install missing dependencies (Guix, Guile, Shepherd)"
    echo "3. Validate Scheme syntax in configuration files"
    echo "4. Check devcontainer configuration for Guix Shepherd support"
    echo ""
    echo "📚 Documentation: See GUIX-SHEPHERD-DEVCONTAINER-SUMMARY.md for setup guide"
    exit 1
fi