
;; constants
;;;; title: Decentralized-Micro-Lending
;; version: 1.0
;; summary: A decentralized micro-lending platform
;; description: Allows users to request loans by staking collateral, lenders to fund loans, and handles repayments and liquidations

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-LOAN-NOT-FOUND (err u101))
(define-constant ERR-LOAN-ALREADY-FUNDED (err u102))
(define-constant ERR-INSUFFICIENT-COLLATERAL (err u103))
(define-constant ERR-LOAN-NOT-FUNDED (err u104))
(define-constant ERR-LOAN-ALREADY-REPAID (err u105))
(define-constant ERR-LOAN-NOT-DEFAULTED (err u106))
(define-constant ERR-LOAN-ALREADY-LIQUIDATED (err u107))
(define-constant ERR-INVALID-AMOUNT (err u108))
(define-constant ERR-NOT-BORROWER (err u109))
(define-constant ERR-NOT-LENDER (err u110))

;; Minimum collateral ratio (150%)
(define-constant MIN-COLLATERAL-RATIO u150)

;; Default period in blocks (approximately 2 weeks)
(define-constant DEFAULT-PERIOD u2016)

;; Data vars
(define-data-var next-loan-id uint u1)
(define-data-var platform-fee-percentage uint u1) ;; 1% fee

;; Loan status enum: 0=Requested, 1=Funded, 2=Repaid, 3=Defaulted, 4=Liquidated
(define-data-var total-loans-count uint u0)
(define-data-var total-active-loans uint u0)
(define-data-var total-repaid-loans uint u0)
(define-data-var total-defaulted-loans uint u0)
(define-data-var total-liquidated-loans uint u0)

;; Data maps
(define-map loans
  { loan-id: uint }
  {
    borrower: principal,
    amount: uint,
    collateral: uint,
    interest-rate: uint, ;; in basis points (e.g., 500 = 5%)
    term-length: uint, ;; in blocks
    status: uint,
    lender: (optional principal),
    funded-at: (optional uint),
    repaid-at: (optional uint),
    liquidated-at: (optional uint)
  }
)

(define-map user-loans
  { user: principal }
  { loan-ids: (list 20 uint) }
)

(define-map user-funded-loans
  { user: principal }
  { loan-ids: (list 20 uint) }
)

;; Public functions

;; Request a loan
(define-public (request-loan (amount uint) (collateral uint) (interest-rate uint) (term-length uint))
  (let
    (
      (loan-id (var-get next-loan-id))
      (collateral-ratio (/ (* collateral u100) amount))
      (user-loan-ids (default-to { loan-ids: (list u0) } (map-get? user-loans { user: tx-sender })))
    )
    ;; Check if collateral is sufficient
    (asserts! (>= collateral-ratio MIN-COLLATERAL-RATIO) ERR-INSUFFICIENT-COLLATERAL)
    ;; Check if amount is valid
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    ;; Check if term length is valid
    (asserts! (> term-length u0) ERR-INVALID-AMOUNT)
    
    ;; Transfer collateral to contract
    (try! (stx-transfer? collateral tx-sender (as-contract tx-sender)))
    
    ;; Create loan
    (map-set loans
      { loan-id: loan-id }
      {
        borrower: tx-sender,
        amount: amount,
        collateral: collateral,
        interest-rate: interest-rate,
        term-length: term-length,
        status: u0, ;; Requested
        lender: none,
        funded-at: none,
        repaid-at: none,
        liquidated-at: none
      }
    )
    
    ;; Update user loans
    (map-set user-loans
      { user: tx-sender }
      { loan-ids: (unwrap! (as-max-len? (append (get loan-ids user-loan-ids) loan-id) u20) ERR-INVALID-AMOUNT) }
    )
    
    ;; Increment loan ID
    (var-set next-loan-id (+ loan-id u1))
    (var-set total-loans-count (+ (var-get total-loans-count) u1))
    (var-set total-active-loans (+ (var-get total-active-loans) u1))
    
    (ok loan-id)
  )
)
;; Fund a loan
(define-public (fund-loan (loan-id uint))
  (let
    (
      (loan (unwrap! (map-get? loans { loan-id: loan-id }) ERR-LOAN-NOT-FOUND))
      (lender-funded-loans (default-to { loan-ids: (list) } (map-get? user-funded-loans { user: tx-sender })))
      (new-loan-list (unwrap! (as-max-len? (append (get loan-ids lender-funded-loans) loan-id) u20) ERR-INVALID-AMOUNT))
    )
    ;; Check if loan is still in requested status
    (asserts! (is-eq (get status loan) u0) ERR-LOAN-ALREADY-FUNDED)
    
    ;; Transfer funds to borrower
    (try! (stx-transfer? (get amount loan) tx-sender (get borrower loan)))
    
    ;; Update loan status
    (map-set loans
      { loan-id: loan-id }
      (merge loan {
        status: u1, ;; Funded
        lender: (some tx-sender),
        funded-at: (some stacks-block-height)
      })
    )
    
    ;; Update lender's funded loans
    (map-set user-funded-loans
      { user: tx-sender }
      { loan-ids: new-loan-list }
    )
    
    (ok true)
  )
)

;; Repay a loan
(define-public (repay-loan (loan-id uint))
  (let
    (
      (loan (unwrap! (map-get? loans { loan-id: loan-id }) ERR-LOAN-NOT-FOUND))
      (lender (unwrap! (get lender loan) ERR-LOAN-NOT-FUNDED))
      (interest-amount (/ (* (get amount loan) (get interest-rate loan)) u10000))
      (total-repayment (+ (get amount loan) interest-amount))
      (platform-fee (/ (* total-repayment (var-get platform-fee-percentage)) u100))
      (lender-amount (- total-repayment platform-fee))
    )
    ;; Check if caller is the borrower
    (asserts! (is-eq tx-sender (get borrower loan)) ERR-NOT-BORROWER)
    ;; Check if loan is funded
    (asserts! (is-eq (get status loan) u1) ERR-LOAN-NOT-FUNDED)
    
    ;; Transfer repayment to lender
    (try! (stx-transfer? lender-amount tx-sender lender))
    
    ;; Transfer fee to contract owner
    (try! (stx-transfer? platform-fee tx-sender CONTRACT-OWNER))
    
    ;; Return collateral to borrower
    (try! (as-contract (stx-transfer? (get collateral loan) tx-sender (get borrower loan))))
    
    ;; Update loan status
    (map-set loans
      { loan-id: loan-id }
      (merge loan {
        status: u2, ;; Repaid
        repaid-at: (some stacks-block-height)
      })
    )
    
    (var-set total-active-loans (- (var-get total-active-loans) u1))
    (var-set total-repaid-loans (+ (var-get total-repaid-loans) u1))
    
    (ok true)
  )
)

;; Check if a loan is in default
(define-public (check-loan-default (loan-id uint))
  (let
    (
      (loan (unwrap! (map-get? loans { loan-id: loan-id }) ERR-LOAN-NOT-FOUND))
      (funded-at (unwrap! (get funded-at loan) ERR-LOAN-NOT-FUNDED))
      (term-end (+ funded-at (get term-length loan)))
    )
    ;; Check if loan is funded and not already repaid or liquidated
    (asserts! (is-eq (get status loan) u1) ERR-LOAN-NOT-FUNDED)
    ;; Check if loan term has ended
    (asserts! (>= stacks-block-height term-end) ERR-LOAN-NOT-DEFAULTED)
    
    ;; Update loan status to defaulted
    (map-set loans
      { loan-id: loan-id }
      (merge loan { status: u3 }) ;; Defaulted
    )
    
    (var-set total-defaulted-loans (+ (var-get total-defaulted-loans) u1))
    
    (ok true)
  )
)

;; Liquidate a defaulted loan
(define-public (liquidate-loan (loan-id uint))
  (let
    (
      (loan (unwrap! (map-get? loans { loan-id: loan-id }) ERR-LOAN-NOT-FOUND))
      (lender (unwrap! (get lender loan) ERR-LOAN-NOT-FUNDED))
    )
    ;; Check if caller is the lender
    (asserts! (is-eq tx-sender lender) ERR-NOT-LENDER)
    ;; Check if loan is in defaulted status
    (asserts! (is-eq (get status loan) u3) ERR-LOAN-NOT-DEFAULTED)
    
    ;; Transfer collateral to lender
    (try! (as-contract (stx-transfer? (get collateral loan) tx-sender lender)))
    
    ;; Update loan status
    (map-set loans
      { loan-id: loan-id }
      (merge loan {
        status: u4, ;; Liquidated
        liquidated-at: (some stacks-block-height)
      })
    )
    
    (var-set total-active-loans (- (var-get total-active-loans) u1))
    (var-set total-liquidated-loans (+ (var-get total-liquidated-loans) u1))
    
    (ok true)
  )
)
;; Cancel a loan request (only if not funded yet)
(define-public (cancel-loan-request (loan-id uint))
  (let
    (
      (loan (unwrap! (map-get? loans { loan-id: loan-id }) ERR-LOAN-NOT-FOUND))
    )
    ;; Check if caller is the borrower
    (asserts! (is-eq tx-sender (get borrower loan)) ERR-NOT-BORROWER)
    ;; Check if loan is still in requested status
    (asserts! (is-eq (get status loan) u0) ERR-LOAN-ALREADY-FUNDED)
    
    ;; Return collateral to borrower
    (try! (as-contract (stx-transfer? (get collateral loan) tx-sender (get borrower loan))))
    
    ;; Update loan status (we'll use repaid status for simplicity)
    (map-set loans
      { loan-id: loan-id }
      (merge loan {
        status: u2, ;; Repaid (cancelled)
        repaid-at: (some stacks-block-height)
      })
    )
    
    (var-set total-active-loans (- (var-get total-active-loans) u1))
    (var-set total-repaid-loans (+ (var-get total-repaid-loans) u1))
    
    (ok true)
  )
)
;; Read-only functions

;; Get loan details
(define-read-only (get-loan (loan-id uint))
  (map-get? loans { loan-id: loan-id })
)

;; Get user's loans
(define-read-only (get-user-loans (user principal))
  (map-get? user-loans { user: user })
)

;; Get user's funded loans
(define-read-only (get-user-funded-loans (user principal))
  (map-get? user-funded-loans { user: user })
)

;; Calculate collateral ratio
(define-read-only (calculate-collateral-ratio (loan-id uint))
  (let
    (
      (loan (unwrap! (map-get? loans { loan-id: loan-id }) ERR-LOAN-NOT-FOUND))
    )
    (ok (/ (* (get collateral loan) u100) (get amount loan)))
  )
)

;; Get platform statistics
(define-read-only (get-platform-stats)
  {
    total-loans: (var-get total-loans-count),
    active-loans: (var-get total-active-loans),
    repaid-loans: (var-get total-repaid-loans),
    defaulted-loans: (var-get total-defaulted-loans),
    liquidated-loans: (var-get total-liquidated-loans)
  }
)

;; Admin functions

;; Update platform fee (only contract owner)
(define-public (update-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (<= new-fee u10) ERR-INVALID-AMOUNT) ;; Max 10% fee
    (var-set platform-fee-percentage new-fee)
    (ok true)
  )
)



(define-constant ERR-EXTENSION-NOT-ALLOWED (err u111))
(define-constant EXTENSION-FEE-PERCENTAGE u5)
(define-constant MAX-EXTENSIONS u2)

(define-map loan-extensions 
  { loan-id: uint }
  { extension-count: uint }
)

(define-public (extend-loan (loan-id uint) (additional-blocks uint))
  (let
    (
      (loan (unwrap! (map-get? loans { loan-id: loan-id }) ERR-LOAN-NOT-FOUND))
      (extension-data (default-to { extension-count: u0 } (map-get? loan-extensions { loan-id: loan-id })))
      (extension-fee (/ (* (get amount loan) EXTENSION-FEE-PERCENTAGE) u100))
    )
    (asserts! (is-eq tx-sender (get borrower loan)) ERR-NOT-BORROWER)
    (asserts! (is-eq (get status loan) u1) ERR-LOAN-NOT-FUNDED)
    (asserts! (< (get extension-count extension-data) MAX-EXTENSIONS) ERR-EXTENSION-NOT-ALLOWED)
    
    (try! (stx-transfer? extension-fee tx-sender CONTRACT-OWNER))
    
    (map-set loans
      { loan-id: loan-id }
      (merge loan { term-length: (+ (get term-length loan) additional-blocks) })
    )
    
    (map-set loan-extensions
      { loan-id: loan-id }
      { extension-count: (+ (get extension-count extension-data) u1) }
    )
    
    (ok true)
  )
)


(define-map borrower-stats
  { borrower: principal }
  {
    loans-taken: uint,
    loans-repaid: uint,
    loans-defaulted: uint
  }
)

(define-read-only (get-loan-risk-score (loan-id uint))
  (let
    (
      (loan (unwrap! (map-get? loans { loan-id: loan-id }) ERR-LOAN-NOT-FOUND))
      (borrower-data (default-to { loans-taken: u0, loans-repaid: u0, loans-defaulted: u0 } 
                     (map-get? borrower-stats { borrower: (get borrower loan) })))
      (collateral-ratio (/ (* (get collateral loan) u100) (get amount loan)))
      (repayment-ratio (if (is-eq (get loans-taken borrower-data) u0)
                          u100
                          (/ (* (get loans-repaid borrower-data) u100) (get loans-taken borrower-data))))
    )
    (ok {
      risk-score: (/ (+ collateral-ratio repayment-ratio) u2),
      collateral-ratio: collateral-ratio,
      repayment-history: repayment-ratio
    })
  )
)

(define-public (update-borrower-stats (borrower principal) (status uint))
  (let
    (
      (stats (default-to { loans-taken: u0, loans-repaid: u0, loans-defaulted: u0 }
              (map-get? borrower-stats { borrower: borrower })))
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    
    (map-set borrower-stats
      { borrower: borrower }
      (merge stats
        {
          loans-taken: (+ (get loans-taken stats) u1),
          loans-repaid: (if (is-eq status u2) (+ (get loans-repaid stats) u1) (get loans-repaid stats)),
          loans-defaulted: (if (is-eq status u3) (+ (get loans-defaulted stats) u1) (get loans-defaulted stats))
        }
      )
    )
    (ok true)
  )
)


(define-constant ERR-INTEREST-CALCULATION-FAILED (err u112))
(define-constant COMPOUNDING-FREQUENCY u144)

(define-map loan-interest-data
  { loan-id: uint }
  {
    principal-amount: uint,
    accrued-interest: uint,
    last-compound-block: uint,
    compound-count: uint
  }
)

(define-data-var total-accrued-interest uint u0)

(define-public (initialize-compound-interest (loan-id uint))
  (let
    (
      (loan (unwrap! (map-get? loans { loan-id: loan-id }) ERR-LOAN-NOT-FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq (get status loan) u1) ERR-LOAN-NOT-FUNDED)
    (asserts! (is-none (map-get? loan-interest-data { loan-id: loan-id })) ERR-LOAN-ALREADY-FUNDED)
    
    (map-set loan-interest-data
      { loan-id: loan-id }
      {
        principal-amount: (get amount loan),
        accrued-interest: u0,
        last-compound-block: current-block,
        compound-count: u0
      }
    )
    (ok true)
  )
)

(define-public (compound-loan-interest (loan-id uint))
  (let
    (
      (loan (unwrap! (map-get? loans { loan-id: loan-id }) ERR-LOAN-NOT-FOUND))
      (interest-data (unwrap! (map-get? loan-interest-data { loan-id: loan-id }) ERR-LOAN-NOT-FOUND))
      (current-block stacks-block-height)
      (blocks-since-last-compound (- current-block (get last-compound-block interest-data)))
      (periods-to-compound (/ blocks-since-last-compound COMPOUNDING-FREQUENCY))
    )
    (asserts! (is-eq (get status loan) u1) ERR-LOAN-NOT-FUNDED)
    (asserts! (> periods-to-compound u0) ERR-INTEREST-CALCULATION-FAILED)
    
    (let
      (
        (current-principal (+ (get principal-amount interest-data) (get accrued-interest interest-data)))
        (period-rate (/ (get interest-rate loan) u10000))
        (compound-multiplier (+ u10000 period-rate))
        (new-amount (/ (* current-principal compound-multiplier) u10000))
        (new-interest (- new-amount (get principal-amount interest-data)))
        (interest-increase (- new-interest (get accrued-interest interest-data)))
      )
      
      (map-set loan-interest-data
        { loan-id: loan-id }
        {
          principal-amount: (get principal-amount interest-data),
          accrued-interest: new-interest,
          last-compound-block: current-block,
          compound-count: (+ (get compound-count interest-data) periods-to-compound)
        }
      )
      
      (var-set total-accrued-interest (+ (var-get total-accrued-interest) interest-increase))
      (ok new-interest)
    )
  )
)

(define-public (repay-compound-loan (loan-id uint))
  (let
    (
      (loan (unwrap! (map-get? loans { loan-id: loan-id }) ERR-LOAN-NOT-FOUND))
      (interest-data (unwrap! (map-get? loan-interest-data { loan-id: loan-id }) ERR-LOAN-NOT-FOUND))
      (lender (unwrap! (get lender loan) ERR-LOAN-NOT-FUNDED))
    )
    (asserts! (is-eq tx-sender (get borrower loan)) ERR-NOT-BORROWER)
    (asserts! (is-eq (get status loan) u1) ERR-LOAN-NOT-FUNDED)
    
    (try! (compound-loan-interest loan-id))
    
    (let
      (
        (updated-interest-data (unwrap! (map-get? loan-interest-data { loan-id: loan-id }) ERR-LOAN-NOT-FOUND))
        (total-repayment (+ (get principal-amount updated-interest-data) (get accrued-interest updated-interest-data)))
        (platform-fee (/ (* total-repayment (var-get platform-fee-percentage)) u100))
        (lender-amount (- total-repayment platform-fee))
      )
      
      (try! (stx-transfer? lender-amount tx-sender lender))
      (try! (stx-transfer? platform-fee tx-sender CONTRACT-OWNER))
      (try! (as-contract (stx-transfer? (get collateral loan) tx-sender (get borrower loan))))
      
      (map-set loans
        { loan-id: loan-id }
        (merge loan {
          status: u2,
          repaid-at: (some stacks-block-height)
        })
      )
      
      (var-set total-active-loans (- (var-get total-active-loans) u1))
      (var-set total-repaid-loans (+ (var-get total-repaid-loans) u1))
      
      (ok total-repayment)
    )
  )
)

(define-read-only (get-compound-interest-data (loan-id uint))
  (map-get? loan-interest-data { loan-id: loan-id })
)

(define-read-only (calculate-current-compound-amount (loan-id uint))
  (let
    (
      (loan (unwrap! (map-get? loans { loan-id: loan-id }) ERR-LOAN-NOT-FOUND))
      (interest-data (unwrap! (map-get? loan-interest-data { loan-id: loan-id }) ERR-LOAN-NOT-FOUND))
      (current-block stacks-block-height)
      (blocks-since-last-compound (- current-block (get last-compound-block interest-data)))
      (periods-to-compound (/ blocks-since-last-compound COMPOUNDING-FREQUENCY))
    )
    (if (is-eq periods-to-compound u0)
      (ok (+ (get principal-amount interest-data) (get accrued-interest interest-data)))
      (let
        (
          (current-principal (+ (get principal-amount interest-data) (get accrued-interest interest-data)))
          (period-rate (/ (get interest-rate loan) u10000))
          (compound-multiplier (+ u10000 period-rate))
          (new-amount (/ (* current-principal compound-multiplier) u10000))
        )
        (ok new-amount)
      )
    )
  )
)

(define-read-only (get-total-platform-interest)
  (var-get total-accrued-interest)
)

(define-read-only (get-loan-compound-summary (loan-id uint))
  (let
    (
      (interest-data (map-get? loan-interest-data { loan-id: loan-id }))
      (current-amount (calculate-current-compound-amount loan-id))
    )
    (match interest-data
      data (ok {
        principal: (get principal-amount data),
        accrued-interest: (get accrued-interest data),
        current-total: (unwrap-panic current-amount),
        compound-periods: (get compound-count data),
        last-compound-block: (get last-compound-block data)
      })
      ERR-LOAN-NOT-FOUND
    )
  )
)