;; Shepherd service definitions for OpenCog Guix packaging
;; This file defines services for building, testing, and managing OpenCog packages

(use-modules (shepherd service)
             (shepherd support))

;; Service for building OpenCog packages
(define opencog-build-service
  (service
   '(opencog-build)
   #:start (make-forkexec-constructor 
            '("guix" "build" "-f" "/workspace/opencog.scm"))
   #:stop (make-kill-destructor)
   #:description "Build OpenCog package using Guix"
   #:one-shot? #t))

;; Service for testing OpenCog packages  
(define opencog-test-service
  (service
   '(opencog-test)
   #:start (make-forkexec-constructor
            '("sh" "-c" "cd /workspace && guix build -f opencog.scm && echo 'OpenCog build successful'"))
   #:stop (make-kill-destructor)
   #:description "Test OpenCog package build"
   #:one-shot? #t
   #:requirement '(opencog-build)))

;; Service for cogutil vendoring and build
(define cogutil-vendor-service
  (service
   '(cogutil-vendor)
   #:start (make-forkexec-constructor
            '("sh" "-c" "cd /workspace && ./demo-guix-vendor-integration.sh"))
   #:stop (make-kill-destructor)
   #:description "Vendor cogutil and validate integration"
   #:one-shot? #t))

;; Service for package development environment
(define opencog-dev-env-service
  (service
   '(opencog-dev-env)
   #:start (make-forkexec-constructor
            '("guix" "shell" "--manifest=/workspace/cognitive-manifest.scm" "--" "bash"))
   #:stop (make-kill-destructor)
   #:description "Start OpenCog development environment shell"))

;; Service for continuous package building
(define opencog-watch-service
  (service
   '(opencog-watch)
   #:start (make-forkexec-constructor
            '("sh" "-c" "while true; do inotifywait -e modify /workspace/opencog.scm && herd restart opencog-build; done"))
   #:stop (make-kill-destructor)
   #:description "Watch for changes and rebuild OpenCog packages"
   #:requirement '(opencog-build)))

;; Register all services
(register-services opencog-build-service
                   opencog-test-service
                   cogutil-vendor-service
                   opencog-dev-env-service
                   opencog-watch-service)

;; Start core services
(start-service 'cogutil-vendor)
(start-service 'opencog-build)