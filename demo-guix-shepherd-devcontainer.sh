#!/bin/bash
# Demo script for using the GNU Guix Shepherd devcontainer setup

echo "🧪 GNU Guix Shepherd DevContainer Demo"
echo "======================================"
echo ""

echo "This demonstration shows the complete GNU Guix Shepherd devcontainer setup for"
echo "turning an OpenCog repository into Guix packages, as requested in the issue."
echo ""

echo "📁 Files Created:"
echo "=================="
echo ""

echo "1. DevContainer Configuration:"
echo "   - .devcontainer/Dockerfile          # Debian + Guix + Shepherd setup"
echo "   - .devcontainer/devcontainer.json   # VSCode devcontainer configuration"
echo ""

echo "2. OpenCog Package Definition:"
echo "   - opencog.scm                       # Complete OpenCog Guix package definition"
echo ""

echo "3. Shepherd Service Management:"
echo "   - .config/shepherd/init.scm         # Shepherd service definitions"
echo ""

echo "4. Build Profile Integration:"
echo "   - base-devcontainers.scm            # Added Shepherd packaging profile"
echo ""

echo "🔧 Usage Instructions:"
echo "======================"
echo ""

echo "1. Open in VSCode with Remote-Containers extension:"
echo "   - VSCode will detect the .devcontainer configuration"
echo "   - Choose 'Reopen in Container' when prompted"
echo "   - Container will build with Debian + Guix + Shepherd"
echo ""

echo "2. Inside the container, start Shepherd:"
echo "   shepherd --config=/workspace/.config/shepherd/init.scm"
echo ""

echo "3. Use Shepherd services for OpenCog packaging:"
echo "   herd start opencog-build      # Build OpenCog package"
echo "   herd start opencog-test       # Test OpenCog package"
echo "   herd start cogutil-vendor     # Vendor cogutil dependencies"
echo "   herd start opencog-dev-env    # Start development environment"
echo "   herd start opencog-watch      # Watch for changes and rebuild"
echo ""

echo "4. Direct Guix packaging commands:"
echo "   guix build -f opencog.scm     # Build OpenCog package directly"
echo "   guix install -f opencog.scm   # Install OpenCog package"
echo "   guix shell --manifest=cognitive-manifest.scm  # Development shell"
echo ""

echo "🎯 Key Features Implemented:"
echo "============================"
echo ""

echo "✅ Portable: Clone repo → Open in VSCode → Get full Guix+Shepherd environment"
echo "✅ Reproducible: Container and Guix manage all dependencies declaratively"
echo "✅ FSF-Sanctioned: Pure Debian/Guix, AGPL, GNU tools only"
echo "✅ Scheme Madness: Shepherd services for build/test automation"
echo "✅ Package Management: Complete OpenCog package definitions"
echo "✅ Service Orchestration: Automated builds, tests, and monitoring"
echo ""

echo "📋 Package Definitions Include:"
echo "==============================="
echo ""

echo "- opencog: Main OpenCog AGI framework package"
echo "- cogutil: OpenCog utilities library"  
echo "- atomspace: AtomSpace knowledge representation"
echo "- All packages configured with:"
echo "  * Git source checkout from OpenCog repositories"
echo "  * CMake build system"
echo "  * Python and Guile bindings"
echo "  * Proper dependency management"
echo ""

echo "🚀 Next Steps:"
echo "=============="
echo ""

echo "1. Open this repository in VSCode with Remote-Containers"
echo "2. Wait for container to build (first time only)"
echo "3. Start Shepherd services for package management"
echo "4. Begin OpenCog development with full Guix reproducibility!"
echo ""

echo "This setup transforms your repo into a 'mad scientist's Guix packaging lab'"
echo "as specified in the issue, with complete GNU/FSF compliance! 🧬🔬"