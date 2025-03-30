;; ReliefChain: Blockchain-Based Emergency Response Network
;; A transparent and efficient disaster response fund distribution system

;; Define NFT Trait
(define-trait nft-trait
    (
        (transfer (uint principal principal) (response bool uint))
        (get-owner (uint) (response principal uint))
        (get-last-token-id () (response uint uint))
        (get-token-uri (uint) (response (optional (string-ascii 256)) uint))
    )
)

;; Constants
(define-constant contract-admin tx-sender)
(define-constant minimum-contribution u100000)
(define-constant approval-threshold u75)
(define-constant metadata-uri "ipfs://relief-chain/metadata/")
(define-constant validation-requirement u3)

;; Error Constants
(define-constant err-unauthorized (err u100))
(define-constant err-crisis-inactive (err u101))
(define-constant err-insufficient-balance (err u102))
(define-constant err-invalid-contribution (err u103))
(define-constant err-aid-plan-completed (err u104))
(define-constant err-transfer-failed (err u105))
(define-constant err-not-token-owner (err u106))
(define-constant err-token-not-found (err u107))
(define-constant err-already-registered (err u108))
(define-constant err-invalid-validation (err u109))
(define-constant err-not-validated (err u110))

;; Data Variables
(define-data-var pool-balance uint u0)
(define-data-var current-crisis-id uint u0)
(define-data-var latest-token-id uint u0)
(define-data-var latest-beneficiary-id uint u0)

;; Data Maps
(define-map contributors 
    principal 
    {total-contributed: uint, 
     governance-power: uint, 
     certificate-count: uint})

(define-map crises 
    uint 
    {title: (string-ascii 64), 
     magnitude: uint, 
     needed-resources: uint, 
     total-distributed: uint, 
     active: bool})

(define-map aid-plans
    uint 
    {description: (string-ascii 256),
     resources: uint,
     support: uint,
     implemented: bool})

(define-map beneficiaries
    uint
    {principal: principal,
     crisis-id: uint,
     region: (string-ascii 64),
     impact-level: uint,
     validated: bool,
     validation-count: uint,
     encrypted-data: (string-ascii 1024),
     evidence: (string-ascii 1024)})

(define-map beneficiary-validations
    {beneficiary-id: uint, validator: principal}
    bool)

(define-map trusted-validators
    principal
    bool)

(define-map token-uris
    uint 
    (string-ascii 256))

(define-map token-owners
    uint
    principal)

;; NFT Implementation
(define-non-fungible-token relief-certificate uint)

;; Read-Only Functions
(define-read-only (get-contributor-info (contributor principal))
    (default-to 
        {total-contributed: u0, governance-power: u0, certificate-count: u0}
        (map-get? contributors contributor)))

(define-read-only (get-crisis-info (crisis-id uint))
    (map-get? crises crisis-id))

(define-read-only (get-beneficiary-info (beneficiary-id uint))
    (map-get? beneficiaries beneficiary-id))

(define-read-only (get-validation-status (beneficiary-id uint))
    (let ((beneficiary (unwrap! (get-beneficiary-info beneficiary-id) (ok false))))
        (ok (get validated beneficiary))))

(define-read-only (get-pool-balance)
    (var-get pool-balance))

(define-read-only (get-token-owner (token-id uint))
    (ok (map-get? token-owners token-id)))

(define-read-only (get-token-uri (token-id uint))
    (ok (map-get? token-uris token-id)))

(define-read-only (get-latest-token-id)
    (ok (var-get latest-token-id)))

;; Beneficiary Registration Functions
(define-public (register-as-beneficiary 
    (crisis-id uint)
    (region (string-ascii 64))
    (impact-level uint)
    (encrypted-data (string-ascii 1024))
    (evidence (string-ascii 1024)))
    (let ((beneficiary-id (+ (var-get latest-beneficiary-id) u1))
          (crisis (unwrap! (get-crisis-info crisis-id) err-crisis-inactive)))
        (if (get active crisis)
            (begin
                (var-set latest-beneficiary-id beneficiary-id)
                (map-set beneficiaries beneficiary-id
                    {principal: tx-sender,
                     crisis-id: crisis-id,
                     region: region,
                     impact-level: impact-level,
                     validated: false,
                     validation-count: u0,
                     encrypted-data: encrypted-data,
                     evidence: evidence})
                (ok beneficiary-id))
            err-crisis-inactive)))

;; Validator Management
(define-public (register-validator (validator principal))
    (if (is-eq tx-sender contract-admin)
        (begin
            (map-set trusted-validators validator true)
            (ok true))
        err-unauthorized))

(define-public (validate-beneficiary (beneficiary-id uint))
    (let (
        (beneficiary (unwrap! (get-beneficiary-info beneficiary-id) err-unauthorized))
        (is-validator (default-to false (map-get? trusted-validators tx-sender)))
        (has-validated (default-to false (map-get? beneficiary-validations {beneficiary-id: beneficiary-id, validator: tx-sender})))
        )
        (if (and is-validator (not has-validated))
            (begin
                (map-set beneficiary-validations {beneficiary-id: beneficiary-id, validator: tx-sender} true)
                (map-set beneficiaries beneficiary-id
                    (merge beneficiary 
                        {validation-count: (+ (get validation-count beneficiary) u1),
                         validated: (>= (+ (get validation-count beneficiary) u1) validation-requirement)}))
                (ok true))
            err-unauthorized)))

;; Contribution Function
(define-public (contribute)
    (let ((amount (stx-get-balance tx-sender))
          (contributor-info (get-contributor-info tx-sender)))
        (if (>= amount minimum-contribution)
            (begin
                (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
                (map-set contributors tx-sender
                    {total-contributed: (+ (get total-contributed contributor-info) amount),
                     governance-power: (+ (get governance-power contributor-info) amount),
                     certificate-count: (+ (get certificate-count contributor-info) u1)})
                (var-set pool-balance (+ (var-get pool-balance) amount))
                (let ((new-token-id (+ (var-get latest-token-id) u1)))
                    (var-set latest-token-id new-token-id)
                    (try! (nft-mint? relief-certificate new-token-id tx-sender))
                    (map-set token-owners new-token-id tx-sender)
                    (map-set token-uris new-token-id metadata-uri)
                    (ok true)))
            err-invalid-contribution)))

(define-public (register-crisis (title (string-ascii 64)) (magnitude uint) (needed-resources uint))
    (let ((crisis-id (+ (var-get current-crisis-id) u1)))
        (if (is-eq tx-sender contract-admin)
            (begin
                (map-set crises crisis-id
                    {title: title,
                     magnitude: magnitude,
                     needed-resources: needed-resources,
                     total-distributed: u0,
                     active: true})
                (var-set current-crisis-id crisis-id)
                (ok crisis-id))
            err-unauthorized)))

(define-public (create-aid-plan (crisis-id uint) (description (string-ascii 256)) (resources uint))
    (let ((crisis (unwrap! (get-crisis-info crisis-id) err-crisis-inactive)))
        (if (and 
                (get active crisis)
                (<= resources (var-get pool-balance)))
            (begin
                (map-set aid-plans crisis-id
                    {description: description,
                     resources: resources,
                     support: u0,
                     implemented: false})
                (ok true))
            err-insufficient-balance)))

(define-public (vote-on-aid-plan (crisis-id uint))
    (let ((plan (unwrap! (map-get? aid-plans crisis-id) err-crisis-inactive))
          (contributor-info (get-contributor-info tx-sender)))
        (if (not (get implemented plan))
            (begin
                (map-set aid-plans crisis-id
                    (merge plan {support: (+ (get support plan) (get governance-power contributor-info))}))
                (ok true))
            err-aid-plan-completed)))

(define-public (transfer (token-id uint) (sender principal) (recipient principal))
    (let ((token-owner (unwrap! (map-get? token-owners token-id) err-token-not-found)))
        (if (and
                (is-eq tx-sender sender)
                (is-eq token-owner sender))
            (begin
                (map-set token-owners token-id recipient)
                (ok true))
            err-not-token-owner)))

(define-public (update-crisis-magnitude (crisis-id uint) (new-magnitude uint))
    (let ((crisis (unwrap! (get-crisis-info crisis-id) err-crisis-inactive)))
        (if (is-eq tx-sender contract-admin)
            (begin
                (map-set crises crisis-id
                    (merge crisis {magnitude: new-magnitude})) 
                (ok true))
            err-unauthorized)))