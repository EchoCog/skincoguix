#!/bin/bash

# test-ci-workflows.sh
# Comprehensive testing script for all GitHub Action workflows
# Tests each workflow to determine success/failure status

set -e

# Color coding for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[PASS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[FAIL]${NC} $1"
}

log_placeholder() {
    echo -e "${RED}[PLACEHOLDER/MOCK]${NC} $1"
}

# Test results tracking
declare -A workflow_results
declare -A workflow_details
declare -A placeholder_workflows

WORKSPACE_ROOT="/home/runner/work/skincoguix/skincoguix"
WORKFLOWS_DIR="$WORKSPACE_ROOT/.github/workflows"
TEMPLATES_DIR="$WORKSPACE_ROOT/templates"

echo "=============================================="
echo "🧪 CI WORKFLOW COMPREHENSIVE TESTING"
echo "=============================================="
echo ""

# Test 1: Validate workflow file syntax
test_workflow_syntax() {
    local workflow_file="$1"
    local workflow_name=$(basename "$workflow_file" .yml)
    
    log_info "Testing syntax for $workflow_name..."
    
    # Check if file exists and is readable
    if [ ! -f "$workflow_file" ]; then
        workflow_results["$workflow_name"]="FAIL"
        workflow_details["$workflow_name"]="File not found"
        log_error "Workflow file not found: $workflow_file"
        return 1
    fi
    
    # Basic YAML syntax validation
    if command -v python3 >/dev/null 2>&1; then
        if python3 -c "import yaml; yaml.safe_load(open('$workflow_file'))" 2>/dev/null; then
            log_success "YAML syntax valid for $workflow_name"
        else
            workflow_results["$workflow_name"]="FAIL"
            workflow_details["$workflow_name"]="Invalid YAML syntax"
            log_error "Invalid YAML syntax in $workflow_name"
            return 1
        fi
    fi
    
    # Check for placeholders/mocks (worse than failures)
    local placeholder_patterns=(
        "TODO"
        "FIXME" 
        "PLACEHOLDER"
        "MOCK"
        "echo.*placeholder"
        "echo.*mock"
        "echo.*TODO"
        "run:.*#.*placeholder"
        "run:.*#.*mock"
    )
    
    local has_placeholders=false
    for pattern in "${placeholder_patterns[@]}"; do
        if grep -iq "$pattern" "$workflow_file"; then
            placeholder_workflows["$workflow_name"]="true"
            has_placeholders=true
            log_placeholder "Found placeholder/mock content in $workflow_name: $pattern"
        fi
    done
    
    if [ "$has_placeholders" = true ]; then
        workflow_results["$workflow_name"]="PLACEHOLDER"
        workflow_details["$workflow_name"]="Contains placeholder/mock implementations"
        return 1
    fi
    
    workflow_results["$workflow_name"]="PASS"
    workflow_details["$workflow_name"]="Syntax validation passed"
    return 0
}

# Test 2: Analyze workflow structure and dependencies
test_workflow_structure() {
    local workflow_file="$1"
    local workflow_name=$(basename "$workflow_file" .yml)
    
    log_info "Analyzing structure for $workflow_name..."
    
    # Check for required workflow elements
    local required_elements=(
        "name:"
        "on:"
        "jobs:"
    )
    
    local missing_elements=()
    for element in "${required_elements[@]}"; do
        if ! grep -q "^$element" "$workflow_file"; then
            missing_elements+=("$element")
        fi
    done
    
    if [ ${#missing_elements[@]} -gt 0 ]; then
        workflow_details["$workflow_name"]="${workflow_details["$workflow_name"]} Missing elements: ${missing_elements[*]}"
        log_warning "Missing required elements in $workflow_name: ${missing_elements[*]}"
    fi
    
    # Check for specific dependency patterns
    if grep -q "repos/" "$workflow_file"; then
        log_success "$workflow_name uses monorepo structure"
    else
        log_warning "$workflow_name may not be updated for monorepo structure"
    fi
    
    # Check for common CI issues
    if grep -q "git config.*safe.directory" "$workflow_file"; then
        log_success "$workflow_name includes git safe.directory fix"
    else
        log_warning "$workflow_name missing git safe.directory configuration"
    fi
    
    return 0
}

# Test 3: Component-specific workflow analysis
test_component_workflows() {
    log_info "Analyzing component-specific workflows..."
    
    # Map workflows to components
    declare -A component_workflows
    component_workflows["oc.yml"]="main opencog build"
    component_workflows["guix.yml"]="guix integration"
    component_workflows["cognitive-ecosystem.yml"]="cognitive ecosystem"
    component_workflows["efficient-build.yml"]="efficient build"
    component_workflows["ci-org-v7.yml"]="organization ci v7"
    component_workflows["guix-vendor-integration.yml"]="guix vendor integration"
    
    for workflow in "${!component_workflows[@]}"; do
        local workflow_path="$WORKFLOWS_DIR/$workflow"
        local component="${component_workflows[$workflow]}"
        
        if [ -f "$workflow_path" ]; then
            log_info "Found workflow for $component: $workflow"
            test_workflow_syntax "$workflow_path"
            test_workflow_structure "$workflow_path"
        else
            log_error "Missing workflow for $component: $workflow"
        fi
    done
}

# Test 4: Repository component coverage
test_component_coverage() {
    log_info "Testing component coverage..."
    
    local repos_dir="$WORKSPACE_ROOT/repos"
    local components=()
    
    if [ -d "$repos_dir" ]; then
        while IFS= read -r -d '' component; do
            components+=("$(basename "$component")")
        done < <(find "$repos_dir" -maxdepth 1 -type d -not -name "." -print0)
    fi
    
    log_info "Found ${#components[@]} OpenCog components"
    
    # Check which components have dedicated workflows
    local covered_components=0
    local uncovered_components=()
    
    for component in "${components[@]}"; do
        local has_workflow=false
        
        # Check if any workflow specifically mentions this component
        for workflow_file in "$WORKFLOWS_DIR"/*.yml; do
            if grep -q "$component" "$workflow_file" 2>/dev/null; then
                has_workflow=true
                ((covered_components++))
                log_success "Component $component has workflow coverage"
                break
            fi
        done
        
        if [ "$has_workflow" = false ]; then
            uncovered_components+=("$component")
            log_warning "Component $component lacks dedicated workflow"
        fi
    done
    
    log_info "Components with workflow coverage: $covered_components/${#components[@]}"
    
    if [ ${#uncovered_components[@]} -gt 0 ]; then
        log_warning "Uncovered components: ${uncovered_components[*]}"
    fi
    
    return 0
}

# Main execution
main() {
    echo "Starting comprehensive CI workflow testing..."
    echo ""
    
    # Test all workflow files
    for workflow_file in "$WORKFLOWS_DIR"/*.yml; do
        if [ -f "$workflow_file" ]; then
            echo "----------------------------------------"
            test_workflow_syntax "$workflow_file"
            test_workflow_structure "$workflow_file"
            echo ""
        fi
    done
    
    echo "----------------------------------------"
    test_component_workflows
    echo ""
    
    echo "----------------------------------------"
    test_component_coverage
    echo ""
    
    # Generate summary report
    echo "=============================================="
    echo "📊 WORKFLOW TESTING SUMMARY"
    echo "=============================================="
    
    local total_workflows=0
    local passed_workflows=0
    local failed_workflows=0
    local placeholder_count=0
    
    for workflow in "${!workflow_results[@]}"; do
        ((total_workflows++))
        case "${workflow_results[$workflow]}" in
            "PASS")
                ((passed_workflows++))
                echo -e "${GREEN}✅ $workflow${NC}: ${workflow_details[$workflow]}"
                ;;
            "FAIL")
                ((failed_workflows++))
                echo -e "${RED}❌ $workflow${NC}: ${workflow_details[$workflow]}"
                ;;
            "PLACEHOLDER")
                ((placeholder_count++))
                ((failed_workflows++)) # Placeholders count as failures
                echo -e "${RED}🚨 $workflow${NC}: ${workflow_details[$workflow]} (WORSE THAN FAILURE)"
                ;;
        esac
    done
    
    echo ""
    echo "📈 Summary Statistics:"
    echo "  Total Workflows: $total_workflows"
    echo "  Passed: $passed_workflows"
    echo "  Failed: $failed_workflows"
    echo "  Placeholder/Mock: $placeholder_count"
    echo ""
    
    if [ $placeholder_count -gt 0 ]; then
        echo -e "${RED}⚠️  WARNING: $placeholder_count workflows contain placeholder/mock implementations${NC}"
        echo -e "${RED}   These are worse than failures as they don't provide real testing${NC}"
    fi
    
    if [ $failed_workflows -gt 0 ]; then
        echo -e "${RED}❌ $failed_workflows workflows have issues that need attention${NC}"
        return 1
    else
        echo -e "${GREEN}✅ All workflows passed validation${NC}"
        return 0
    fi
}

# Run the main function
main "$@"