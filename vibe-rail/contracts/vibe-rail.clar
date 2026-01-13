;; VibeRail - Decentralized Randomness Service
;; A three-tier randomness infrastructure for blockchain applications

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-insufficient-payment (err u101))
(define-constant err-invalid-tier (err u102))
(define-constant err-request-not-found (err u103))
(define-constant err-already-fulfilled (err u104))
(define-constant err-unauthorized (err u105))

;; Randomness tiers
(define-constant tier-basic u1)
(define-constant tier-standard u2)
(define-constant tier-quantum u3)

;; Pricing per tier (in microSTX)
(define-constant price-basic u1000000)
(define-constant price-standard u5000000)
(define-constant price-quantum u10000000)

;; Data Variables
(define-data-var request-nonce uint u0)
(define-data-var total-requests uint u0)
(define-data-var insurance-pool uint u0)

;; Data Maps
(define-map randomness-requests
  uint
  {
    requester: principal,
    tier: uint,
    fulfilled: bool,
    random-value: (optional uint),
    block-height: uint,
    timestamp: uint
  }
)

(define-map validator-nodes
  principal
  {
    active: bool,
    stake: uint,
    fulfilled-count: uint
  }
)

(define-map subscription-tiers
  principal
  {
    tier: uint,
    balance: uint,
    requests-remaining: uint
  }
)

;; Read-only functions
(define-read-only (get-request (request-id uint))
  (map-get? randomness-requests request-id)
)

(define-read-only (get-validator (validator principal))
  (map-get? validator-nodes validator)
)

(define-read-only (get-subscription (user principal))
  (map-get? subscription-tiers user)
)

(define-read-only (get-tier-price (tier uint))
  (if (is-eq tier tier-basic)
    (ok price-basic)
    (if (is-eq tier tier-standard)
      (ok price-standard)
      (if (is-eq tier tier-quantum)
        (ok price-quantum)
        err-invalid-tier
      )
    )
  )
)

(define-read-only (get-insurance-pool)
  (ok (var-get insurance-pool))
)

(define-read-only (get-total-requests)
  (ok (var-get total-requests))
)

;; Private functions
(define-private (generate-entropy (seed uint))
  (let
    (
      (block-hash (unwrap! (get-block-info? id-header-hash (- block-height u1)) u0))
      (combined (+ seed (mod (len block-hash) u1000000)))
    )
    (mod combined u1000000000)
  )
)

;; Public functions

;; Register as a validator node
(define-public (register-validator (stake-amount uint))
  (let
    (
      (validator tx-sender)
    )
    (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
    (map-set validator-nodes validator
      {
        active: true,
        stake: stake-amount,
        fulfilled-count: u0
      }
    )
    (ok true)
  )
)

;; Request randomness
(define-public (request-randomness (tier uint))
  (let
    (
      (request-id (var-get request-nonce))
      (price (unwrap! (get-tier-price tier) err-invalid-tier))
    )
    ;; Transfer payment
    (try! (stx-transfer? price tx-sender (as-contract tx-sender)))
    
    ;; Create request
    (map-set randomness-requests request-id
      {
        requester: tx-sender,
        tier: tier,
        fulfilled: false,
        random-value: none,
        block-height: block-height,
        timestamp: block-height
      }
    )
    
    ;; Update counters
    (var-set request-nonce (+ request-id u1))
    (var-set total-requests (+ (var-get total-requests) u1))
    
    (ok request-id)
  )
)

;; Fulfill randomness request (validator only)
(define-public (fulfill-randomness (request-id uint))
  (let
    (
      (request (unwrap! (map-get? randomness-requests request-id) err-request-not-found))
      (validator-info (unwrap! (map-get? validator-nodes tx-sender) err-unauthorized))
      (random-seed (+ request-id block-height (var-get total-requests)))
      (random-value (generate-entropy random-seed))
    )
    ;; Check if already fulfilled
    (asserts! (not (get fulfilled request)) err-already-fulfilled)
    
    ;; Check validator is active
    (asserts! (get active validator-info) err-unauthorized)
    
    ;; Update request with random value
    (map-set randomness-requests request-id
      (merge request {
        fulfilled: true,
        random-value: (some random-value)
      })
    )
    
    ;; Update validator stats
    (map-set validator-nodes tx-sender
      (merge validator-info {
        fulfilled-count: (+ (get fulfilled-count validator-info) u1)
      })
    )
    
    (ok random-value)
  )
)

;; Subscribe to a tier with prepayment
(define-public (subscribe-tier (tier uint) (num-requests uint))
  (let
    (
      (price (unwrap! (get-tier-price tier) err-invalid-tier))
      (total-cost (* price num-requests))
    )
    (try! (stx-transfer? total-cost tx-sender (as-contract tx-sender)))
    
    (map-set subscription-tiers tx-sender
      {
        tier: tier,
        balance: total-cost,
        requests-remaining: num-requests
      }
    )
    
    (ok true)
  )
)

;; Stake to insurance pool
(define-public (stake-insurance (amount uint))
  (begin
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set insurance-pool (+ (var-get insurance-pool) amount))
    (ok true)
  )
)

;; Withdraw from insurance pool (owner only)
(define-public (withdraw-insurance (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (try! (as-contract (stx-transfer? amount tx-sender recipient)))
    (var-set insurance-pool (- (var-get insurance-pool) amount))
    (ok true)
  )
)

;; Deactivate validator
(define-public (deactivate-validator)
  (let
    (
      (validator-info (unwrap! (map-get? validator-nodes tx-sender) err-unauthorized))
    )
    (map-set validator-nodes tx-sender
      (merge validator-info { active: false })
    )
    (ok true)
  )
)