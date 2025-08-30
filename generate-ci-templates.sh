#!/bin/bash

# generate-ci-templates.sh
# Generate CI build success/failure templates for each OpenCog component

set -e

# Color coding for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

WORKSPACE_ROOT="/home/runner/work/skincoguix/skincoguix"
REPOS_DIR="$WORKSPACE_ROOT/repos"
TEMPLATES_DIR="$WORKSPACE_ROOT/templates"
WORKFLOWS_DIR="$WORKSPACE_ROOT/.github/workflows"

echo "=============================================="
echo "🏗️  CI BUILD TEMPLATE GENERATOR"
echo "=============================================="

# Generate component template
generate_component_template() {
    local component="$1"
    local template_file="$TEMPLATES_DIR/components/${component}-ci-status.md"
    
    cat > "$template_file" << EOF
# CI Build Status: $component

## Component Information
- **Component Name**: $component
- **Repository Path**: \`repos/$component\`
- **Last Updated**: $(date -u +"%Y-%m-%d %H:%M:%S UTC")

## CI Build Status

### Current Status
- [ ] ✅ Success - All builds passing
- [ ] ⚠️  Warning - Some issues but builds complete
- [ ] ❌ Failure - Builds failing
- [ ] 🚨 Placeholder/Mock - Worse than failure (no real testing)

### Workflow Coverage
- [ ] Has dedicated workflow file
- [ ] Included in main OpenCog build (\`oc.yml\`)
- [ ] Covered by efficient build (\`efficient-build.yml\`)
- [ ] Part of Guix integration (\`guix.yml\`)

## Build Details

### Last Successful Build
- **Date**: [YYYY-MM-DD HH:MM:SS UTC]
- **Commit**: [commit-hash]
- **Workflow**: [workflow-name]
- **Duration**: [build-time]

### Last Failed Build
- **Date**: [YYYY-MM-DD HH:MM:SS UTC]
- **Commit**: [commit-hash]
- **Workflow**: [workflow-name]
- **Error Type**: [error-category]

## Issues Identified

### Build Issues
- [ ] Compilation errors
- [ ] Missing dependencies
- [ ] Test failures
- [ ] Environment problems

### Infrastructure Issues
- [ ] Git ownership problems
- [ ] Container issues
- [ ] Database connection problems
- [ ] Network/timeout issues

### Placeholder/Mock Issues
- [ ] TODO comments in critical paths
- [ ] FIXME markers in workflow steps
- [ ] Mock implementations instead of real tests
- [ ] Placeholder echo statements

## Dependencies

### Build Dependencies
\`\`\`
# List build dependencies here
\`\`\`

### Runtime Dependencies
\`\`\`
# List runtime dependencies here
\`\`\`

## Test Coverage

### Unit Tests
- [ ] Unit tests exist
- [ ] Unit tests passing
- [ ] Coverage threshold met

### Integration Tests
- [ ] Integration tests exist
- [ ] Integration tests passing
- [ ] End-to-end testing included

## Recommended Actions

### Immediate Fixes
- [ ] Fix compilation errors
- [ ] Install missing dependencies
- [ ] Resolve placeholder implementations
- [ ] Add git safe.directory configuration

### Long-term Improvements
- [ ] Add comprehensive test coverage
- [ ] Implement proper error handling
- [ ] Add performance monitoring
- [ ] Update documentation

## Related Issues
- [List GitHub issues related to this component's CI]

## Notes
[Additional notes and context about this component's CI status]
EOF

    echo "Generated template for $component: $template_file"
}

# Generate workflow template
generate_workflow_template() {
    local workflow="$1"
    local template_file="$TEMPLATES_DIR/workflows/${workflow}-status.md"
    
    cat > "$template_file" << EOF
# Workflow Status: $workflow

## Workflow Information
- **Workflow File**: \`.github/workflows/$workflow\`
- **Last Updated**: $(date -u +"%Y-%m-%d %H:%M:%S UTC")

## Status Overview

### Current Status
- [ ] ✅ Success - All jobs passing
- [ ] ⚠️  Warning - Some jobs have issues
- [ ] ❌ Failure - Critical jobs failing
- [ ] 🚨 Placeholder/Mock - Contains placeholder implementations

### Validation Results
- [ ] YAML syntax valid
- [ ] Required elements present (name, on, jobs)
- [ ] No placeholder/mock content detected
- [ ] Proper error handling implemented

## Job Analysis

### Jobs Status
EOF

    # Analyze workflow file if it exists
    local workflow_path="$WORKFLOWS_DIR/$workflow"
    if [ -f "$workflow_path" ]; then
        echo "| Job Name | Status | Issues |" >> "$template_file"
        echo "|----------|---------|---------|" >> "$template_file"
        
        # Extract job names (basic parsing)
        grep -E "^  [a-zA-Z0-9_-]+:" "$workflow_path" | sed 's/://g' | sed 's/^  //' | while read -r job; do
            echo "| $job | ⏳ Pending Analysis | [Check manually] |" >> "$template_file"
        done
    else
        echo "❌ **Workflow file not found**" >> "$template_file"
    fi

    cat >> "$template_file" << EOF

## Issues Detected

### Syntax Issues
- [ ] Invalid YAML syntax
- [ ] Missing required fields
- [ ] Malformed job definitions

### Placeholder/Mock Content
- [ ] TODO comments in workflow steps
- [ ] FIXME markers
- [ ] Placeholder echo statements
- [ ] Mock implementations

### Configuration Issues
- [ ] Missing git safe.directory
- [ ] Incorrect container images
- [ ] Missing environment variables
- [ ] Improper dependency setup

## Component Coverage

### Components Tested
- [List OpenCog components tested by this workflow]

### Components Missing
- [List OpenCog components that should be but aren't tested]

## Performance Metrics

### Build Times
- **Average**: [minutes]
- **Last Run**: [minutes]
- **Trend**: [improving/stable/degrading]

### Success Rate
- **Last 10 runs**: [X/10]
- **Last 30 days**: [X%]
- **Overall**: [X%]

## Recommendations

### Immediate Actions
- [ ] Fix syntax errors
- [ ] Remove placeholder content
- [ ] Add missing configuration
- [ ] Update component coverage

### Future Improvements
- [ ] Optimize build performance
- [ ] Add better error reporting
- [ ] Implement caching strategies
- [ ] Add monitoring and alerts

## Related Documentation
- [CI Build Fixes Documentation](../../CI-BUILD-FIXES.md)
- [Issue Template](../../.github/ISSUE_TEMPLATE/ci-build-failure.md)

## Notes
[Additional notes about this workflow]
EOF

    echo "Generated template for workflow $workflow: $template_file"
}

# Generate CI build report based on the issue template
generate_ci_build_report() {
    local component="$1"
    local report_file="$TEMPLATES_DIR/reports/${component}-ci-build-report.md"
    
    cat > "$report_file" << EOF
# CI Build Success / (Failure) Report: $component

**Generated**: $(date -u +"%Y-%m-%d %H:%M:%S UTC")
**Component**: $component

## CI Build Success / (Failure) Report

### **Issue Type**
- [ ] Compilation Error
- [ ] Test Failure
- [ ] Dependency Issue
- [ ] Environment Problem
- [ ] Database Connection Issue
- [ ] Placeholder/Mock Implementation (worse than failure)
- [ ] Other

### **Workflow Affected**
- [ ] \`oc.yml\` - Main OpenCog build
- [ ] \`guix.yml\` - Guix integration
- [ ] \`cognitive-ecosystem.yml\` - Cognitive ecosystem
- [ ] \`efficient-build.yml\` - Efficient build
- [ ] Other: _______________

### **Error Details**

#### **Error Message**
\`\`\`
[Paste the exact error message here]
\`\`\`

#### **Build Log Location**
- **Workflow Run**: [Link to failed workflow run]
- **Job**: [Job name]
- **Step**: [Step name]
- **Timestamp**: [When the error occurred]

### **Root Cause Analysis**

#### **1. Git Ownership Issues**
- [ ] \`fatal: detected dubious ownership in repository\`
- **Solution**: Add git safe.directory configuration
- **Status**: [ ] Fixed [ ] Pending [ ] Not Applicable

#### **2. Missing Dependencies**
- [ ] Valgrind not found
- [ ] CMake package missing
- [ ] Compiler toolchain issue
- **Solution**: Install missing packages
- **Status**: [ ] Fixed [ ] Pending [ ] Not Applicable

#### **3. Compilation Errors**
- [ ] Missing include files
- [ ] Undefined symbols
- [ ] Template instantiation errors
- **Solution**: Add missing includes or fix code
- **Status**: [ ] Fixed [ ] Pending [ ] Not Applicable

#### **4. Database Issues**
- [ ] Role "root" does not exist
- [ ] Connection refused
- [ ] Authentication failed
- **Solution**: Fix database configuration
- **Status**: [ ] Fixed [ ] Pending [ ] Not Applicable

#### **5. Placeholder/Mock Issues**
- [ ] TODO comments blocking builds
- [ ] FIXME markers in critical paths
- [ ] Mock implementations instead of real tests
- **Solution**: Replace with real implementations
- **Status**: [ ] Fixed [ ] Pending [ ] Not Applicable

### **Steps to Reproduce**
1. [Step 1]
2. [Step 2]
3. [Step 3]

### **Expected Behavior**
[Describe what should happen]

### **Actual Behavior**
[Describe what actually happened]

### **Environment Information**
- **OS**: Ubuntu (GitHub-hosted runner)
- **Runner**: GitHub-hosted
- **Container**: [Docker image used]
- **Git Version**: [Git version]
- **CMake Version**: [CMake version]

### **Proposed Solutions**

#### **Immediate Fixes**
- [ ] Add git safe.directory configuration
- [ ] Install missing dependencies (valgrind, etc.)
- [ ] Add missing include statements
- [ ] Fix database user/role configuration
- [ ] Replace placeholder/mock implementations
- [ ] Update CMake configuration

#### **Long-term Improvements**
- [ ] Add dependency checking in workflow
- [ ] Implement better error handling
- [ ] Add build validation steps
- [ ] Improve documentation
- [ ] Add comprehensive testing

### **Files Modified**
- [ ] \`.github/workflows/[workflow-name].yml\`
- [ ] \`repos/$component/[file]\`
- [ ] \`CMakeLists.txt\`
- [ ] Other: _______________

### **Testing**
- [ ] Local build successful
- [ ] CI build successful
- [ ] Tests passing
- [ ] Documentation updated

### **Additional Notes**
Component: $component
Repository Path: repos/$component
Template Generated: $(date -u +"%Y-%m-%d %H:%M:%S UTC")

---

## 🔧 Common Solutions

### **Git Ownership Fix**
\`\`\`bash
git config --global --add safe.directory \${{ github.workspace }}
git config --global --add safe.directory /__w/ocguix/ocguix
\`\`\`

### **Valgrind Installation**
\`\`\`bash
sudo apt-get update && sudo apt-get install -y valgrind
\`\`\`

### **Missing Include Fix**
\`\`\`cpp
#include <opencog/util/iostreamContainer.h>
\`\`\`

### **Database Role Fix**
\`\`\`sql
CREATE ROLE opencog_test;
GRANT ALL PRIVILEGES ON DATABASE atomspace_db TO opencog_test;
\`\`\`

### **Placeholder/Mock Replacement**
Replace TODO/FIXME/MOCK implementations with real functionality:
- Remove placeholder echo statements
- Implement actual test logic
- Add proper error handling
- Use real dependencies instead of mocks

---

**Labels**: \`ci\`, \`build-failure\`, \`bug\`, \`$component\`
**Priority**: [High/Medium/Low]
**Estimated Effort**: [1-2 hours / 1 day / 1 week]
EOF

    echo "Generated CI build report for $component: $report_file"
}

# Main execution
main() {
    echo "Starting CI template generation..."
    echo ""
    
    # Get list of OpenCog components
    local components=()
    if [ -d "$REPOS_DIR" ]; then
        while IFS= read -r -d '' component_dir; do
            local component=$(basename "$component_dir")
            components+=("$component")
        done < <(find "$REPOS_DIR" -maxdepth 1 -type d -not -name "." -print0)
    fi
    
    echo "Found ${#components[@]} OpenCog components"
    
    # Generate component templates
    echo ""
    echo "Generating component CI status templates..."
    for component in "${components[@]}"; do
        generate_component_template "$component"
        generate_ci_build_report "$component"
    done
    
    # Generate workflow templates
    echo ""
    echo "Generating workflow status templates..."
    for workflow_file in "$WORKFLOWS_DIR"/*.yml; do
        if [ -f "$workflow_file" ]; then
            local workflow=$(basename "$workflow_file")
            generate_workflow_template "$workflow"
        fi
    done
    
    echo ""
    echo "=============================================="
    echo "✅ Template generation complete!"
    echo "=============================================="
    echo "Generated templates:"
    echo "  - Component templates: ${#components[@]}"
    echo "  - Workflow templates: $(ls "$WORKFLOWS_DIR"/*.yml | wc -l)"
    echo "  - CI build reports: ${#components[@]}"
    echo ""
    echo "Templates location: $TEMPLATES_DIR"
    echo "  - templates/components/ - Component CI status templates"
    echo "  - templates/workflows/ - Workflow status templates" 
    echo "  - templates/reports/ - CI build success/failure reports"
}

# Run the main function
main "$@"