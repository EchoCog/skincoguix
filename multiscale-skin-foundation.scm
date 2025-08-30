;; multiscale-skin-foundation.scm
;;
;; Core foundation for multiscale skin modeling
;; Defines basic data structures, scales, and entities for skin representation
;;
;; This file establishes the foundational AtomSpace schemas for representing
;; skin structure and function across molecular, cellular, tissue, and organ scales

(use-modules (opencog) (opencog exec))

;; =============================================================================
;; MULTISCALE HIERARCHY DEFINITION
;; =============================================================================

;; Define the four primary scales for skin modeling
(define-public molecular-scale (ConceptNode "MolecularScale"))
(define-public cellular-scale (ConceptNode "CellularScale"))  
(define-public tissue-scale (ConceptNode "TissueScale"))
(define-public organ-scale (ConceptNode "OrganScale"))

;; Establish hierarchical relationships between scales
;; Each scale inherits properties from the scale below it
(InheritanceLink molecular-scale cellular-scale)
(InheritanceLink cellular-scale tissue-scale)
(InheritanceLink tissue-scale organ-scale)

;; Define scale containment relationships
;; Elements at each scale are contained within the next higher scale
(define-public scale-contains (PredicateNode "ScaleContains"))

(EvaluationLink
    scale-contains
    (ListLink cellular-scale molecular-scale))

(EvaluationLink
    scale-contains
    (ListLink tissue-scale cellular-scale))

(EvaluationLink
    scale-contains
    (ListLink organ-scale tissue-scale))

;; =============================================================================
;; MOLECULAR SCALE ENTITIES
;; =============================================================================

;; Core structural proteins
(define-public keratin (ConceptNode "Keratin"))
(define-public collagen (ConceptNode "Collagen"))
(define-public elastin (ConceptNode "Elastin"))
(define-public fibronectin (ConceptNode "Fibronectin"))
(define-public laminin (ConceptNode "Laminin"))

;; Lipid components
(define-public ceramide (ConceptNode "Ceramide"))
(define-public cholesterol (ConceptNode "Cholesterol"))
(define-public fatty-acid (ConceptNode "FattyAcid"))
(define-public phospholipid (ConceptNode "Phospholipid"))

;; Important molecules for skin function
(define-public hyaluronic-acid (ConceptNode "HyaluronicAcid"))
(define-public melanin (ConceptNode "Melanin"))
(define-public vitamin-d (ConceptNode "VitaminD"))
(define-public water-molecule (ConceptNode "WaterMolecule"))

;; Associate molecular entities with molecular scale
(InheritanceLink keratin molecular-scale)
(InheritanceLink collagen molecular-scale)
(InheritanceLink elastin molecular-scale)
(InheritanceLink ceramide molecular-scale)
(InheritanceLink melanin molecular-scale)

;; =============================================================================
;; CELLULAR SCALE ENTITIES
;; =============================================================================

;; Primary skin cell types
(define-public keratinocyte (ConceptNode "Keratinocyte"))
(define-public fibroblast (ConceptNode "Fibroblast"))
(define-public melanocyte (ConceptNode "Melanocyte"))
(define-public langerhans-cell (ConceptNode "LangerhansCell"))
(define-public merkel-cell (ConceptNode "MerkelCell"))

;; Specialized cell states
(define-public basal-keratinocyte (ConceptNode "BasalKeratinocyte"))
(define-public spinous-keratinocyte (ConceptNode "SpinousKeratinocyte"))
(define-public granular-keratinocyte (ConceptNode "GranularKeratinocyte"))
(define-public cornified-keratinocyte (ConceptNode "CornifiedKeratinocyte"))

;; Cell cycle states
(define-public g1-phase (ConceptNode "G1Phase"))
(define-public s-phase (ConceptNode "SPhase"))
(define-public g2-phase (ConceptNode "G2Phase"))
(define-public m-phase (ConceptNode "MPhase"))
(define-public apoptosis (ConceptNode "Apoptosis"))

;; Associate cellular entities with cellular scale
(InheritanceLink keratinocyte cellular-scale)
(InheritanceLink fibroblast cellular-scale)
(InheritanceLink melanocyte cellular-scale)

;; Define cell differentiation hierarchy
(InheritanceLink basal-keratinocyte keratinocyte)
(InheritanceLink spinous-keratinocyte keratinocyte)
(InheritanceLink granular-keratinocyte keratinocyte)
(InheritanceLink cornified-keratinocyte keratinocyte)

;; =============================================================================
;; TISSUE SCALE ENTITIES
;; =============================================================================

;; Primary skin layers
(define-public epidermis (ConceptNode "Epidermis"))
(define-public dermis (ConceptNode "Dermis"))
(define-public hypodermis (ConceptNode "Hypodermis"))

;; Epidermal sublayers
(define-public stratum-basale (ConceptNode "StratumBasale"))
(define-public stratum-spinosum (ConceptNode "StratumSpinosum"))
(define-public stratum-granulosum (ConceptNode "StratumGranulosum"))
(define-public stratum-corneum (ConceptNode "StratumCorneum"))

;; Dermal sublayers
(define-public papillary-dermis (ConceptNode "PapillaryDermis"))
(define-public reticular-dermis (ConceptNode "ReticularDermis"))

;; Specialized tissue structures
(define-public hair-follicle (ConceptNode "HairFollicle"))
(define-public sebaceous-gland (ConceptNode "SebaceousGland"))
(define-public sweat-gland (ConceptNode "SweatGland"))
(define-public blood-vessel (ConceptNode "BloodVessel"))
(define-public nerve-fiber (ConceptNode "NerveFiber"))

;; Associate tissue entities with tissue scale
(InheritanceLink epidermis tissue-scale)
(InheritanceLink dermis tissue-scale)
(InheritanceLink hypodermis tissue-scale)

;; Define tissue containment relationships
(EvaluationLink
    scale-contains
    (ListLink epidermis stratum-basale))

(EvaluationLink
    scale-contains
    (ListLink epidermis stratum-spinosum))

(EvaluationLink
    scale-contains
    (ListLink dermis papillary-dermis))

(EvaluationLink
    scale-contains
    (ListLink dermis reticular-dermis))

;; =============================================================================
;; ORGAN SCALE ENTITIES
;; =============================================================================

;; Skin as a complete organ system
(define-public skin-organ (ConceptNode "SkinOrgan"))

;; Regional skin types
(define-public facial-skin (ConceptNode "FacialSkin"))
(define-public body-skin (ConceptNode "BodySkin"))
(define-public palmar-skin (ConceptNode "PalmarSkin"))
(define-public plantar-skin (ConceptNode "PlantarSkin"))

;; Skin appendages as organ components
(define-public hair-system (ConceptNode "HairSystem"))
(define-public nail-system (ConceptNode "NailSystem"))

;; Associate organ entities with organ scale
(InheritanceLink skin-organ organ-scale)
(InheritanceLink facial-skin skin-organ)
(InheritanceLink body-skin skin-organ)

;; =============================================================================
;; FUNDAMENTAL SKIN FUNCTIONS
;; =============================================================================

;; Primary skin functions
(define-public barrier-function (ConceptNode "BarrierFunction"))
(define-public thermoregulation (ConceptNode "Thermoregulation"))
(define-public sensory-perception (ConceptNode "SensoryPerception"))
(define-public immune-protection (ConceptNode "ImmuneProtection"))
(define-public vitamin-d-synthesis (ConceptNode "VitaminDSynthesis"))

;; Sensory modalities
(define-public tactile-sensation (ConceptNode "TactileSensation"))
(define-public thermal-sensation (ConceptNode "ThermalSensation"))
(define-public pain-sensation (ConceptNode "PainSensation"))
(define-public pressure-sensation (ConceptNode "PressureSensation"))

;; Associate functions with appropriate scales
(InheritanceLink barrier-function organ-scale)
(InheritanceLink thermoregulation organ-scale)
(InheritanceLink sensory-perception organ-scale)

;; =============================================================================
;; MULTISCALE PROPERTIES
;; =============================================================================

;; Define property types that exist across scales
(define-public mechanical-property (ConceptNode "MechanicalProperty"))
(define-public transport-property (ConceptNode "TransportProperty"))
(define-public thermal-property (ConceptNode "ThermalProperty"))
(define-public biochemical-property (ConceptNode "BiochemicalProperty"))

;; Specific property instances
(define-public elasticity (ConceptNode "Elasticity"))
(define-public permeability (ConceptNode "Permeability"))
(define-public thermal-conductivity (ConceptNode "ThermalConductivity"))
(define-public diffusion-coefficient (ConceptNode "DiffusionCoefficient"))

;; Property inheritance across scales
(InheritanceLink elasticity mechanical-property)
(InheritanceLink permeability transport-property)
(InheritanceLink thermal-conductivity thermal-property)

;; =============================================================================
;; SCALE INTERACTION PREDICATES
;; =============================================================================

;; Define predicates for inter-scale relationships
(define-public influences (PredicateNode "Influences"))
(define-public aggregates-to (PredicateNode "AggregatesTo"))
(define-public constrains (PredicateNode "Constrains"))
(define-public emerges-from (PredicateNode "EmergesFrom"))

;; Example relationships (to be expanded)
(EvaluationLink
    influences
    (ListLink collagen elasticity))

(EvaluationLink
    aggregates-to
    (ListLink keratinocyte stratum-corneum))

(EvaluationLink
    emerges-from
    (ListLink barrier-function epidermis))

;; =============================================================================
;; UTILITY FUNCTIONS
;; =============================================================================

;; Function to get all entities at a specific scale
(define-public (get-entities-at-scale scale)
    "Return all entities that belong to the specified scale"
    (cog-get-atoms 'ConceptNode #t
        (lambda (atom)
            (not (null? (cog-chase-link 'InheritanceLink 'ConceptNode atom scale))))))

;; Function to check if an entity belongs to a specific scale
(define-public (entity-at-scale? entity scale)
    "Check if an entity belongs to the specified scale"
    (not (null? (cog-chase-link 'InheritanceLink 'ConceptNode entity scale))))

;; Function to get the scale of an entity
(define-public (get-entity-scale entity)
    "Return the scale that an entity belongs to"
    (cog-chase-link 'InheritanceLink 'ConceptNode entity))

;; Function to get all relationships between two scales
(define-public (get-cross-scale-relationships scale1 scale2)
    "Return all relationships between entities of two different scales"
    (let ((entities1 (get-entities-at-scale scale1))
          (entities2 (get-entities-at-scale scale2)))
        ;; This is a simplified version - would need more complex logic
        ;; to find actual relationships
        (list entities1 entities2)))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

;; Function to initialize the multiscale skin foundation
(define-public (initialize-multiscale-skin-foundation)
    "Initialize the multiscale skin modeling foundation"
    (display "Initializing multiscale skin modeling foundation...\n")
    (display "Scales defined: molecular, cellular, tissue, organ\n")
    (display "Basic entities loaded for each scale\n")
    (display "Foundation ready for scale-specific implementations\n")
    #t)

;; Auto-initialize when this module is loaded
(initialize-multiscale-skin-foundation)