(define-module (opencog-package))
(use-modules (guix packages)
             (guix git-download)
             (guix build-system cmake)
             (guix build-system gnu)
             (gnu packages base)
             (gnu packages cmake)
             (gnu packages cpp)
             (gnu packages guile)
             (gnu packages pkg-config)
             (gnu packages python)
             (gnu packages boost))

(define-public opencog
  (package
    (name "opencog")
    (version "latest-git")
    (source (git-checkout
             (url "https://github.com/opencog/opencog.git")
             (commit "HEAD")))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #t
       #:configure-flags
       '("-DCMAKE_BUILD_TYPE=Release"
         "-DPYTHON_EXTENSION=ON"
         "-DGUILE_EXTENSION=ON")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-dependencies
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Ensure dependencies are found
             #t))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "make" "test")))))))
    (native-inputs
     (list cmake pkg-config))
    (inputs
     (list boost 
           guile-3.0 
           python))
    (propagated-inputs
     (list cogutil atomspace))
    (synopsis "OpenCog AGI Framework")
    (description 
     "The OpenCog cognitive architecturing toolkit provides a framework for
developing artificial general intelligence systems. It includes the AtomSpace
knowledge representation system, reasoning engines, and cognitive agents.")
    (home-page "https://opencog.org/")
    (license gpl3+)))

(define-public cogutil
  (package
    (name "cogutil")
    (version "2.0.4")
    (source (git-checkout
             (url "https://github.com/opencog/cogutil.git")
             (commit "HEAD")))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #t
       #:configure-flags
       '("-DCMAKE_BUILD_TYPE=Release")))
    (native-inputs
     (list cmake pkg-config))
    (inputs
     (list boost guile-3.0))
    (synopsis "OpenCog utilities library")
    (description
     "CogUtil provides basic utilities and infrastructure for OpenCog projects,
including logging, configuration, and basic data structures.")
    (home-page "https://github.com/opencog/cogutil")
    (license agpl3+)))

(define-public atomspace
  (package
    (name "atomspace")
    (version "5.0.4")
    (source (git-checkout
             (url "https://github.com/opencog/atomspace.git")
             (commit "HEAD")))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #t
       #:configure-flags
       '("-DCMAKE_BUILD_TYPE=Release"
         "-DPYTHON_EXTENSION=ON"
         "-DGUILE_EXTENSION=ON")))
    (native-inputs
     (list cmake pkg-config))
    (inputs
     (list boost 
           guile-3.0 
           python
           cogutil))
    (synopsis "OpenCog AtomSpace knowledge representation")
    (description
     "The AtomSpace is a knowledge representation system providing a hypergraph
database with pattern matching, rule engines, and knowledge storage capabilities.")
    (home-page "https://github.com/opencog/atomspace")
    (license agpl3+)))

;; Export the main package
opencog