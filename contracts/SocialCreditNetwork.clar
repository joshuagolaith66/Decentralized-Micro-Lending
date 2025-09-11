;; Social Credit Network Contract
;; Community-based lending through social endorsements and trust relationships

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u500))
(define-constant ERR-INVALID-AMOUNT (err u501))
(define-constant ERR-USER-NOT-FOUND (err u502))
(define-constant ERR-ENDORSEMENT-EXISTS (err u503))
(define-constant ERR-INSUFFICIENT-TRUST (err u504))
(define-constant ERR-SELF-ENDORSEMENT (err u505))
(define-constant ERR-CIRCLE-NOT-FOUND (err u506))
(define-constant ERR-ALREADY-MEMBER (err u507))
(define-constant ERR-ENDORSEMENT-EXPIRED (err u508))

;; Constants
(define-constant MIN-ENDORSEMENTS-FOR-SOCIAL-LOAN u3)
(define-constant MAX-ENDORSEMENTS-PER-USER u10)
(define-constant ENDORSEMENT-VALIDITY-PERIOD u8064) ;; ~8 weeks
(define-constant TRUST-DECAY-FACTOR u95) ;; 5% decay per period
(define-constant SOCIAL-LOAN-DISCOUNT u25) ;; 25% collateral reduction
(define-constant ENDORSER-LIABILITY-PERCENTAGE u10) ;; 10% liability

;; Data variables
(define-data-var total-users uint u0)
(define-data-var total-endorsements uint u0)
(define-data-var circle-counter uint u0)

;; User profile with social metrics
(define-map user-profiles
    principal
    { trust-score: uint,
      endorsement-count: uint,
      endorsements-given: uint,
      successful-loans: uint,
      failed-loans: uint,
      last-activity: uint,
      reputation-tier: uint }
)

;; Social endorsements between users
(define-map social-endorsements
    { endorser: principal, endorsee: principal }
    { trust-level: uint,
      endorsement-message: (string-ascii 200),
      created-at: uint,
      expires-at: uint,
      active: bool,
      liability-accepted: bool }
)

;; Trust circles for group lending
(define-map trust-circles
    uint
    { circle-name: (string-ascii 100),
      organizer: principal,
      members: (list 20 principal),
      member-count: uint,
      total-pool: uint,
      circle-trust-score: uint,
      created-at: uint,
      active: bool }
)

;; Social loan tracking with endorser liability
(define-map social-loans
    uint
    { borrower: principal,
      loan-amount: uint,
      endorsers: (list 10 principal),
      endorser-liabilities: (list 10 uint),
      collateral-discount: uint,
      trust-based: bool,
      status: uint }
)

;; Endorser performance tracking
(define-map endorser-history
    principal
    { total-endorsed: uint,
      successful-endorsements: uint,
      default-losses: uint,
      liability-paid: uint,
      endorser-score: uint }
)

;; Community milestone tracking
(define-map community-milestones
    uint
    { milestone-type: (string-ascii 50),
      target-amount: uint,
      current-amount: uint,
      participants: (list 50 principal),
      reward-pool: uint,
      completed: bool,
      deadline: uint }
)

(define-data-var milestone-counter uint u0)

;; Create or update user profile
(define-public (create-user-profile)
    (let (
        (current-profile (default-to 
            { trust-score: u500, endorsement-count: u0, endorsements-given: u0,
              successful-loans: u0, failed-loans: u0, last-activity: u0, reputation-tier: u1 }
            (map-get? user-profiles tx-sender)))
    )
        (map-set user-profiles tx-sender
            (merge current-profile { last-activity: stacks-block-height }))
        (if (is-eq (get trust-score current-profile) u500)
            (var-set total-users (+ (var-get total-users) u1))
            true)
        (ok true)
    )
)

;; Endorse another user
(define-public (endorse-user 
    (endorsee principal)
    (trust-level uint)
    (message (string-ascii 200))
    (accept-liability bool))
    (let (
        (endorser-profile (unwrap! (map-get? user-profiles tx-sender) ERR-USER-NOT-FOUND))
        (endorsee-profile (unwrap! (map-get? user-profiles endorsee) ERR-USER-NOT-FOUND))
        (endorsement-key { endorser: tx-sender, endorsee: endorsee })
        (expires-at (+ stacks-block-height ENDORSEMENT-VALIDITY-PERIOD))
    )
        (asserts! (not (is-eq tx-sender endorsee)) ERR-SELF-ENDORSEMENT)
        (asserts! (<= trust-level u1000) ERR-INVALID-AMOUNT)
        (asserts! (is-none (map-get? social-endorsements endorsement-key)) ERR-ENDORSEMENT-EXISTS)
        (asserts! (< (get endorsements-given endorser-profile) MAX-ENDORSEMENTS-PER-USER) ERR-INVALID-AMOUNT)
        
        ;; Create endorsement
        (map-set social-endorsements endorsement-key
            { trust-level: trust-level,
              endorsement-message: message,
              created-at: stacks-block-height,
              expires-at: expires-at,
              active: true,
              liability-accepted: accept-liability })
        
        ;; Update profiles
        (map-set user-profiles tx-sender
            (merge endorser-profile { 
                endorsements-given: (+ (get endorsements-given endorser-profile) u1),
                last-activity: stacks-block-height }))
        
        (map-set user-profiles endorsee
            (merge endorsee-profile {
                endorsement-count: (+ (get endorsement-count endorsee-profile) u1),
                trust-score: (+ (get trust-score endorsee-profile) (/ trust-level u10)) }))
        
        (var-set total-endorsements (+ (var-get total-endorsements) u1))
        (ok true)
    )
)

;; Create a trust circle
(define-public (create-trust-circle 
    (name (string-ascii 100))
    (initial-pool uint))
    (let (
        (circle-id (+ (var-get circle-counter) u1))
        (creator-profile (unwrap! (map-get? user-profiles tx-sender) ERR-USER-NOT-FOUND))
    )
        (asserts! (>= (get reputation-tier creator-profile) u3) ERR-INSUFFICIENT-TRUST)
        (asserts! (> initial-pool u0) ERR-INVALID-AMOUNT)
        
        (try! (stx-transfer? initial-pool tx-sender (as-contract tx-sender)))
        
        (map-set trust-circles circle-id
            { circle-name: name,
              organizer: tx-sender,
              members: (list tx-sender),
              member-count: u1,
              total-pool: initial-pool,
              circle-trust-score: (get trust-score creator-profile),
              created-at: stacks-block-height,
              active: true })
        
        (var-set circle-counter circle-id)
        (ok circle-id)
    )
)

;; Join a trust circle
(define-public (join-trust-circle 
    (circle-id uint)
    (contribution uint))
    (let (
        (circle-data (unwrap! (map-get? trust-circles circle-id) ERR-CIRCLE-NOT-FOUND))
        (user-profile (unwrap! (map-get? user-profiles tx-sender) ERR-USER-NOT-FOUND))
        (current-members (get members circle-data))
    )
        (asserts! (get active circle-data) ERR-CIRCLE-NOT-FOUND)
        (asserts! (is-none (index-of current-members tx-sender)) ERR-ALREADY-MEMBER)
        (asserts! (< (len current-members) u20) ERR-INVALID-AMOUNT)
        (asserts! (>= (get trust-score user-profile) u600) ERR-INSUFFICIENT-TRUST)
        (asserts! (> contribution u0) ERR-INVALID-AMOUNT)
        
        (try! (stx-transfer? contribution tx-sender (as-contract tx-sender)))
        
        (map-set trust-circles circle-id
            (merge circle-data {
                members: (unwrap! (as-max-len? (append current-members tx-sender) u20) ERR-INVALID-AMOUNT),
                member-count: (+ (get member-count circle-data) u1),
                total-pool: (+ (get total-pool circle-data) contribution),
                circle-trust-score: (/ (+ (get circle-trust-score circle-data) (get trust-score user-profile)) u2) }))
        
        (ok true)
    )
)

;; Request social loan with reduced collateral
(define-public (request-social-loan 
    (loan-id uint)
    (loan-amount uint))
    (let (
        (borrower-profile (unwrap! (map-get? user-profiles tx-sender) ERR-USER-NOT-FOUND))
        (endorsement-count (get endorsement-count borrower-profile))
        (trust-score (get trust-score borrower-profile))
    )
        (asserts! (>= endorsement-count MIN-ENDORSEMENTS-FOR-SOCIAL-LOAN) ERR-INSUFFICIENT-TRUST)
        (asserts! (>= trust-score u700) ERR-INSUFFICIENT-TRUST)
        (asserts! (> loan-amount u0) ERR-INVALID-AMOUNT)
        
        (let (
            (endorsers-list (get-active-endorsers tx-sender))
            (collateral-discount (calculate-collateral-discount trust-score endorsement-count))
        )
            (map-set social-loans loan-id
                { borrower: tx-sender,
                  loan-amount: loan-amount,
                  endorsers: endorsers-list,
                  endorser-liabilities: (calculate-endorser-liabilities endorsers-list loan-amount),
                  collateral-discount: collateral-discount,
                  trust-based: true,
                  status: u0 })
            
            (ok { discount: collateral-discount, endorsers: (len endorsers-list) })
        )
    )
)

;; Update loan performance and endorser scores
(define-public (update-social-loan-performance 
    (loan-id uint)
    (successful bool))
    (let (
        (social-loan (unwrap! (map-get? social-loans loan-id) ERR-USER-NOT-FOUND))
        (borrower (get borrower social-loan))
        (borrower-profile (unwrap! (map-get? user-profiles borrower) ERR-USER-NOT-FOUND))
    )
        ;; Update borrower profile
        (if successful
            (map-set user-profiles borrower
                (merge borrower-profile {
                    successful-loans: (+ (get successful-loans borrower-profile) u1),
                    trust-score: (+ (get trust-score borrower-profile) u50),
                    reputation-tier: (if (< (+ (get reputation-tier borrower-profile) u1) u5)
                                       (+ (get reputation-tier borrower-profile) u1) u5) }))
            (map-set user-profiles borrower
                (merge borrower-profile {
                    failed-loans: (+ (get failed-loans borrower-profile) u1),
                    trust-score: (if (> (- (get trust-score borrower-profile) u100) u100)
                                   (- (get trust-score borrower-profile) u100) u100) })))
        
        ;; Update endorsers
        (unwrap-panic (update-endorser-scores (get endorsers social-loan) successful))
        
        (map-set social-loans loan-id
            (merge social-loan { status: (if successful u2 u3) }))
        
        (ok true)
    )
)

;; Create community milestone
(define-public (create-community-milestone
    (milestone-type (string-ascii 50))
    (target-amount uint)
    (reward-pool uint)
    (deadline uint))
    (let (
        (milestone-id (+ (var-get milestone-counter) u1))
    )
        (asserts! (> target-amount u0) ERR-INVALID-AMOUNT)
        (asserts! (> deadline stacks-block-height) ERR-INVALID-AMOUNT)
        (asserts! (> reward-pool u0) ERR-INVALID-AMOUNT)
        
        (try! (stx-transfer? reward-pool tx-sender (as-contract tx-sender)))
        
        (map-set community-milestones milestone-id
            { milestone-type: milestone-type,
              target-amount: target-amount,
              current-amount: u0,
              participants: (list),
              reward-pool: reward-pool,
              completed: false,
              deadline: deadline })
        
        (var-set milestone-counter milestone-id)
        (ok milestone-id)
    )
)

;; Helper functions

(define-private (get-active-endorsers (user principal))
    ;; Simplified - returns placeholder list
    (list)
)

(define-private (calculate-collateral-discount (trust-score uint) (endorsements uint))
    (let (
        (base-discount (/ (* trust-score SOCIAL-LOAN-DISCOUNT) u1000))
        (endorsement-bonus (if (< (* endorsements u5) u20) (* endorsements u5) u20))
        (total-discount (+ base-discount endorsement-bonus))
    )
        (if (< total-discount u50) total-discount u50)
    )
)

(define-private (calculate-endorser-liabilities (endorsers (list 10 principal)) (amount uint))
    ;; Simplified - returns empty list for now
    (list)
)

(define-private (update-endorser-scores (endorsers (list 10 principal)) (successful bool))
    ;; Update endorser scores based on loan outcome
    (ok true)
)

;; Read-only functions

(define-read-only (get-user-profile (user principal))
    (map-get? user-profiles user)
)

(define-read-only (get-endorsement (endorser principal) (endorsee principal))
    (map-get? social-endorsements { endorser: endorser, endorsee: endorsee })
)

(define-read-only (get-trust-circle (circle-id uint))
    (map-get? trust-circles circle-id)
)

(define-read-only (get-social-loan (loan-id uint))
    (map-get? social-loans loan-id)
)

(define-read-only (get-community-milestone (milestone-id uint))
    (map-get? community-milestones milestone-id)
)

(define-read-only (get-endorser-history (endorser principal))
    (map-get? endorser-history endorser)
)

(define-read-only (calculate-trust-score (user principal))
    (let (
        (profile (default-to 
            { trust-score: u500, endorsement-count: u0, endorsements-given: u0,
              successful-loans: u0, failed-loans: u0, last-activity: u0, reputation-tier: u1 }
            (map-get? user-profiles user)))
        (base-score (get trust-score profile))
        (success-rate (if (> (get successful-loans profile) u0)
                        (/ (* (get successful-loans profile) u100) 
                           (+ (get successful-loans profile) (get failed-loans profile)))
                        u100))
    )
        (ok (+ base-score (/ (* success-rate u2) u1)))
    )
)

(define-read-only (get-network-stats)
    { total-users: (var-get total-users),
      total-endorsements: (var-get total-endorsements),
      total-circles: (var-get circle-counter),
      total-milestones: (var-get milestone-counter) }
)
