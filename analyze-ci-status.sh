#!/bin/bash

# analyze-ci-status.sh
# Analyze actual CI status and populate templates with real data

set -e

# Color coding for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

WORKSPACE_ROOT="/home/runner/work/skincoguix/skincoguix"
WORKFLOWS_DIR="$WORKSPACE_ROOT/.github/workflows"
TEMPLATES_DIR="$WORKSPACE_ROOT/templates"
REPOS_DIR="$WORKSPACE_ROOT/repos"

# Results tracking
declare -A workflow_status
declare -A component_coverage
declare -A placeholder_issues
declare -A build_issues

echo "=============================================="
echo "🔍 CI STATUS ANALYSIS & TEMPLATE POPULATION"
echo "=============================================="

# Analyze workflow for placeholder/mock content
analyze_workflow_placeholders() {
    local workflow_file="$1"
    local workflow_name=$(basename "$workflow_file" .yml)
    
    echo "[INFO] Analyzing $workflow_name for placeholder content..."
    
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
        "echo \".*\""  # Simple echo statements that might be placeholders
    )
    
    local found_placeholders=()
    for pattern in "${placeholder_patterns[@]}"; do
        if grep -iq "$pattern" "$workflow_file"; then
            local matches=$(grep -in "$pattern" "$workflow_file" | head -3)
            found_placeholders+=("$pattern")
            placeholder_issues["$workflow_name"]+="$pattern: $matches\n"
        fi
    done
    
    if [ ${#found_placeholders[@]} -gt 0 ]; then
        workflow_status["$workflow_name"]="PLACEHOLDER"
        echo -e "${RED}🚨 Found placeholders in $workflow_name: ${found_placeholders[*]}${NC}"
        return 1
    else
        echo -e "${GREEN}✅ No placeholders found in $workflow_name${NC}"
        return 0
    fi
}

# Analyze workflow syntax and structure
analyze_workflow_syntax() {
    local workflow_file="$1"
    local workflow_name=$(basename "$workflow_file" .yml)
    
    echo "[INFO] Analyzing $workflow_name syntax and structure..."
    
    # Check YAML syntax
    if command -v python3 >/dev/null 2>&1; then
        if ! python3 -c "import yaml; yaml.safe_load(open('$workflow_file'))" 2>/dev/null; then
            workflow_status["$workflow_name"]="FAIL"
            build_issues["$workflow_name"]+="Invalid YAML syntax\n"
            echo -e "${RED}❌ Invalid YAML syntax in $workflow_name${NC}"
            return 1
        fi
    fi
    
    # Check required elements
    local required_elements=("name:" "on:" "jobs:")
    local missing_elements=()
    
    for element in "${required_elements[@]}"; do
        if ! grep -q "^$element" "$workflow_file"; then
            missing_elements+=("$element")
        fi
    done
    
    if [ ${#missing_elements[@]} -gt 0 ]; then
        build_issues["$workflow_name"]+="Missing elements: ${missing_elements[*]}\n"
        echo -e "${YELLOW}⚠️  Missing elements in $workflow_name: ${missing_elements[*]}${NC}"
    fi
    
    # Check for monorepo structure usage
    if grep -q "repos/" "$workflow_file"; then
        echo -e "${GREEN}✅ $workflow_name uses monorepo structure${NC}"
    else
        build_issues["$workflow_name"]+="May not be updated for monorepo structure\n"
        echo -e "${YELLOW}⚠️  $workflow_name may not use monorepo structure${NC}"
    fi
    
    # Check for git safe.directory configuration
    if grep -q "git config.*safe.directory" "$workflow_file"; then
        echo -e "${GREEN}✅ $workflow_name includes git safe.directory fix${NC}"
    else
        build_issues["$workflow_name"]+="Missing git safe.directory configuration\n"
        echo -e "${YELLOW}⚠️  $workflow_name missing git safe.directory config${NC}"
    fi
    
    return 0
}

# Analyze component coverage by workflows
analyze_component_coverage() {
    echo "[INFO] Analyzing component coverage across workflows..."
    
    # Get list of components
    local components=()
    if [ -d "$REPOS_DIR" ]; then
        while IFS= read -r -d '' component_dir; do
            local component=$(basename "$component_dir")
            if [ "$component" != "." ]; then
                components+=("$component")
            fi
        done < <(find "$REPOS_DIR" -maxdepth 1 -type d -print0)
    fi
    
    echo "Analyzing coverage for ${#components[@]} components..."
    
    # Check coverage for each component
    for component in "${components[@]}"; do
        local covered_by=()
        
        # Check each workflow for component references
        for workflow_file in "$WORKFLOWS_DIR"/*.yml; do
            if [ -f "$workflow_file" ]; then
                local workflow_name=$(basename "$workflow_file" .yml)
                if grep -q "$component" "$workflow_file" 2>/dev/null; then
                    covered_by+=("$workflow_name")
                fi
            fi
        done
        
        if [ ${#covered_by[@]} -gt 0 ]; then
            component_coverage["$component"]="COVERED: ${covered_by[*]}"
            echo -e "${GREEN}✅ $component covered by: ${covered_by[*]}${NC}"
        else
            component_coverage["$component"]="UNCOVERED"
            echo -e "${YELLOW}⚠️  $component not covered by any workflow${NC}"
        fi
    done
}

# Update component template with analysis results
update_component_template() {
    local component="$1"
    local template_file="$TEMPLATES_DIR/components/${component}-ci-status.md"
    
    if [ ! -f "$template_file" ]; then
        echo "Template not found: $template_file"
        return 1
    fi
    
    # Update status based on analysis
    local status_line=""
    case "${component_coverage[$component]}" in
        COVERED*)
            status_line="- [x] ✅ Success - Component has workflow coverage"
            ;;
        UNCOVERED)
            status_line="- [x] ⚠️  Warning - No dedicated workflow coverage"
            ;;
        *)
            status_line="- [x] ❌ Failure - Analysis incomplete"
            ;;
    esac
    
    # Update the template file
    sed -i "s/- \[ \] ✅ Success - All builds passing/$status_line/" "$template_file"
    
    # Add coverage information
    if [[ "${component_coverage[$component]}" == COVERED* ]]; then
        local workflows=$(echo "${component_coverage[$component]}" | sed 's/COVERED: //')
        sed -i "s/- \[ \] Has dedicated workflow file/- [x] Has workflow coverage: $workflows/" "$template_file"
    fi
    
    echo "Updated template for $component"
}

# Update workflow template with analysis results
update_workflow_template() {
    local workflow="$1"
    local template_file="$TEMPLATES_DIR/workflows/${workflow}-status.md"
    
    if [ ! -f "$template_file" ]; then
        echo "Template not found: $template_file"
        return 1
    fi
    
    # Update status based on analysis
    local status_line=""
    case "${workflow_status[$workflow]}" in
        PLACEHOLDER)
            status_line="- [x] 🚨 Placeholder/Mock - Contains placeholder implementations"
            ;;
        FAIL)
            status_line="- [x] ❌ Failure - Critical jobs failing"
            ;;
        *)
            if [ -n "${build_issues[$workflow]}" ]; then
                status_line="- [x] ⚠️  Warning - Some jobs have issues"
            else
                status_line="- [x] ✅ Success - All jobs passing"
            fi
            ;;
    esac
    
    # Update the template file
    sed -i "s/- \[ \] ✅ Success - All jobs passing/$status_line/" "$template_file"
    
    # Add placeholder issues if found
    if [ -n "${placeholder_issues[$workflow]}" ]; then
        echo "" >> "$template_file"
        echo "### Placeholder Issues Found" >> "$template_file"
        echo "\`\`\`" >> "$template_file"
        echo -e "${placeholder_issues[$workflow]}" >> "$template_file"
        echo "\`\`\`" >> "$template_file"
    fi
    
    # Add build issues if found
    if [ -n "${build_issues[$workflow]}" ]; then
        echo "" >> "$template_file"
        echo "### Build Issues Found" >> "$template_file"
        echo -e "${build_issues[$workflow]}" | sed 's/\\n/\n- /g' | sed 's/^/- /' >> "$template_file"
    fi
    
    echo "Updated template for workflow $workflow"
}

# Generate summary report
generate_summary_report() {
    local summary_file="$TEMPLATES_DIR/CI-STATUS-SUMMARY.md"
    
    cat > "$summary_file" << EOF
# CI Status Summary Report

**Generated**: $(date -u +"%Y-%m-%d %H:%M:%S UTC")

## Overview

This report provides a comprehensive analysis of CI build status across all OpenCog components and GitHub Action workflows.

## Workflow Analysis

### Status Summary
EOF

    # Count workflows by status
    local total_workflows=0
    local placeholder_workflows=0
    local failed_workflows=0
    local warning_workflows=0
    local success_workflows=0
    
    for workflow in "${!workflow_status[@]}"; do
        ((total_workflows++))
        case "${workflow_status[$workflow]}" in
            PLACEHOLDER) ((placeholder_workflows++)) ;;
            FAIL) ((failed_workflows++)) ;;
            *) 
                if [ -n "${build_issues[$workflow]}" ]; then
                    ((warning_workflows++))
                else
                    ((success_workflows++))
                fi
                ;;
        esac
    done
    
    # Add workflow status to summary
    for workflow_file in "$WORKFLOWS_DIR"/*.yml; do
        if [ -f "$workflow_file" ]; then
            local workflow=$(basename "$workflow_file" .yml)
            ((total_workflows++))
            
            case "${workflow_status[$workflow]}" in
                PLACEHOLDER)
                    echo "- 🚨 **$workflow**: PLACEHOLDER/MOCK (worse than failure)" >> "$summary_file"
                    ;;
                FAIL)
                    echo "- ❌ **$workflow**: FAILED" >> "$summary_file"
                    ;;
                *)
                    if [ -n "${build_issues[$workflow]}" ]; then
                        echo "- ⚠️  **$workflow**: WARNING (has issues)" >> "$summary_file"
                    else
                        echo "- ✅ **$workflow**: SUCCESS" >> "$summary_file"
                    fi
                    ;;
            esac
        fi
    done
    
    cat >> "$summary_file" << EOF

### Statistics
- **Total Workflows**: $total_workflows
- **Success**: $success_workflows
- **Warning**: $warning_workflows  
- **Failed**: $failed_workflows
- **Placeholder/Mock**: $placeholder_workflows

## Component Coverage Analysis

EOF

    # Add component coverage information
    local total_components=0
    local covered_components=0
    local uncovered_components=0
    
    for component in "${!component_coverage[@]}"; do
        ((total_components++))
        case "${component_coverage[$component]}" in
            COVERED*)
                ((covered_components++))
                echo "- ✅ **$component**: ${component_coverage[$component]}" >> "$summary_file"
                ;;
            UNCOVERED)
                ((uncovered_components++))
                echo "- ⚠️  **$component**: No workflow coverage" >> "$summary_file"
                ;;
        esac
    done
    
    cat >> "$summary_file" << EOF

### Coverage Statistics
- **Total Components**: $total_components
- **Covered**: $covered_components
- **Uncovered**: $uncovered_components
- **Coverage Rate**: $(( covered_components * 100 / total_components ))%

## Critical Issues

### Placeholder/Mock Implementations (Worse than Failures)
EOF

    if [ $placeholder_workflows -gt 0 ]; then
        echo "⚠️  **$placeholder_workflows workflows contain placeholder/mock implementations**" >> "$summary_file"
        echo "" >> "$summary_file"
        for workflow in "${!placeholder_issues[@]}"; do
            echo "#### $workflow" >> "$summary_file"
            echo "\`\`\`" >> "$summary_file"
            echo -e "${placeholder_issues[$workflow]}" >> "$summary_file"
            echo "\`\`\`" >> "$summary_file"
            echo "" >> "$summary_file"
        done
    else
        echo "✅ No placeholder/mock implementations found" >> "$summary_file"
    fi
    
    cat >> "$summary_file" << EOF

### Uncovered Components
EOF

    if [ $uncovered_components -gt 0 ]; then
        echo "⚠️  **$uncovered_components components lack workflow coverage**" >> "$summary_file"
        echo "" >> "$summary_file"
        for component in "${!component_coverage[@]}"; do
            if [[ "${component_coverage[$component]}" == "UNCOVERED" ]]; then
                echo "- $component" >> "$summary_file"
            fi
        done
    else
        echo "✅ All components have workflow coverage" >> "$summary_file"
    fi
    
    cat >> "$summary_file" << EOF

## Recommendations

### Immediate Actions Required
EOF

    if [ $placeholder_workflows -gt 0 ]; then
        echo "1. **Replace placeholder/mock implementations** in $placeholder_workflows workflows" >> "$summary_file"
    fi
    
    if [ $uncovered_components -gt 0 ]; then
        echo "2. **Add workflow coverage** for $uncovered_components uncovered components" >> "$summary_file"
    fi
    
    if [ $failed_workflows -gt 0 ]; then
        echo "3. **Fix failing workflows**: $failed_workflows workflows have critical issues" >> "$summary_file"
    fi
    
    cat >> "$summary_file" << EOF

### Long-term Improvements
- Implement comprehensive testing for all components
- Add automated placeholder/mock detection
- Create component-specific CI workflows where missing
- Improve error handling and reporting

## Related Documentation
- [CI Build Fixes](../CI-BUILD-FIXES.md)
- [Issue Template](../.github/ISSUE_TEMPLATE/ci-build-failure.md)
- [Component Templates](components/)
- [Workflow Templates](workflows/)
- [Build Reports](reports/)

---
**Report Generated By**: analyze-ci-status.sh  
**Last Updated**: $(date -u +"%Y-%m-%d %H:%M:%S UTC")
EOF

    echo "Generated summary report: $summary_file"
}

# Main execution
main() {
    echo "Starting comprehensive CI status analysis..."
    echo ""
    
    # Analyze all workflows
    for workflow_file in "$WORKFLOWS_DIR"/*.yml; do
        if [ -f "$workflow_file" ]; then
            local workflow_name=$(basename "$workflow_file" .yml)
            echo "----------------------------------------"
            echo "Analyzing workflow: $workflow_name"
            
            analyze_workflow_syntax "$workflow_file"
            analyze_workflow_placeholders "$workflow_file"
            update_workflow_template "$workflow_name"
            echo ""
        fi
    done
    
    echo "----------------------------------------"
    analyze_component_coverage
    echo ""
    
    # Update component templates
    echo "----------------------------------------"
    echo "Updating component templates..."
    for component in "${!component_coverage[@]}"; do
        update_component_template "$component"
    done
    echo ""
    
    # Generate summary report
    echo "----------------------------------------"
    generate_summary_report
    echo ""
    
    echo "=============================================="
    echo "✅ CI STATUS ANALYSIS COMPLETE"
    echo "=============================================="
    echo "Analysis results:"
    echo "  - Workflows analyzed: $(ls "$WORKFLOWS_DIR"/*.yml | wc -l)"
    echo "  - Components analyzed: ${#component_coverage[@]}"
    echo "  - Templates updated: $(find "$TEMPLATES_DIR" -name "*.md" | wc -l)"
    echo "  - Summary report: $TEMPLATES_DIR/CI-STATUS-SUMMARY.md"
}

# Run the main function
main "$@"