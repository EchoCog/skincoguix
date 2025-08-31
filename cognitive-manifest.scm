;; Guix manifest for cognitive ecosystem
;; Use with: guix install -m cognitive-manifest.scm

(specifications->manifest
  '(;; Core build tools
    "gcc-toolchain"
    "cmake"
    "make"
    "pkg-config"
    
    ;; Development dependencies  
    "guile"
    "guile-dev"
    "python"
    "python-pip"
    
    ;; Build dependencies for OpenCog
    "boost"
    "cxxtest" 
    "valgrind"
    "doxygen"
    "graphviz"
    
    ;; System utilities
    "git"
    "curl"
    "wget"
    "tar"
    "gzip"
    
    ;; Additional OpenCog dependencies
    "cython"
    "postgresql"
    
    ;; Meta-cognitive tools
    "emacs"
    "vim"
    
    ;; Guix/Shepherd integration
    "shepherd"))
