# Templates Folder for CI Build Status Tracking

This folder contains success/failure status templates for each OpenCog component and GitHub Action workflow.

## Structure

- `workflows/` - Templates for individual workflow success/failure tracking
- `components/` - Templates for OpenCog component build status
- `reports/` - Generated CI build success/failure reports
- `README.md` - This documentation file

## Purpose

This templates system addresses the issue requirement to:
1. Test each GitHub action to verify which workflows/jobs/steps succeed and which fail
2. Treat placeholder/mock implementations as worse than failures
3. Generate CI build workflows for each OpenCog component
4. Track success/failure status systematically

## Usage

The templates in this folder are used by the CI testing and reporting infrastructure to:
- Document workflow status for each component
- Generate standardized CI build failure reports
- Track which components have working vs failing CI workflows
- Identify placeholder/mock implementations that need real testing