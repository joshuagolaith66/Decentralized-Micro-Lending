
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

(define-constant ERR-BID-TOO-HIGH (err u113))
(define-constant ERR-BID-NOT-FOUND (err u114))
(define-constant ERR-BIDDING-CLOSED (err u115))
(define-constant ERR-INSUFFICIENT-FUNDS (err u116))
(define-constant BID-PERIOD-BLOCKS u1008)

(define-map loan-bids
  { loan-id: uint, lender: principal }
  {
    interest-rate: uint,
    bid-amount: uint,
    bid-at: uint,
    active: bool
  }
)

(define-map loan-bid-list
  { loan-id: uint }
  { lenders: (list 10 principal) }
)

(define-map loan-auction-end
  { loan-id: uint }
  { end-block: uint }
)

(define-public (start-loan-auction (loan-id uint))
  (let
    (
      (loan (unwrap! (map-get? loans { loan-id: loan-id }) ERR-LOAN-NOT-FOUND))
      (auction-end (+ stacks-block-height BID-PERIOD-BLOCKS))
    )
    (asserts! (is-eq tx-sender (get borrower loan)) ERR-NOT-BORROWER)
    (asserts! (is-eq (get status loan) u0) ERR-LOAN-ALREADY-FUNDED)
    
    (map-set loan-auction-end
      { loan-id: loan-id }
      { end-block: auction-end }
    )
    
    (map-set loan-bid-list
      { loan-id: loan-id }
      { lenders: (list) }
    )
    
    (ok auction-end)
  )
)

(define-public (place-bid (loan-id uint) (bid-interest-rate uint))
  (let
    (
      (loan (unwrap! (map-get? loans { loan-id: loan-id }) ERR-LOAN-NOT-FOUND))
      (auction-data (unwrap! (map-get? loan-auction-end { loan-id: loan-id }) ERR-LOAN-NOT-FOUND))
      (bid-list (default-to { lenders: (list) } (map-get? loan-bid-list { loan-id: loan-id })))
      (loan-amount (get amount loan))
    )
    (asserts! (is-eq (get status loan) u0) ERR-LOAN-ALREADY-FUNDED)
    (asserts! (< stacks-block-height (get end-block auction-data)) ERR-BIDDING-CLOSED)
    (asserts! (< bid-interest-rate (get interest-rate loan)) ERR-BID-TOO-HIGH)
    
    (try! (stx-transfer? loan-amount tx-sender (as-contract tx-sender)))
    
    (map-set loan-bids
      { loan-id: loan-id, lender: tx-sender }
      {
        interest-rate: bid-interest-rate,
        bid-amount: loan-amount,
        bid-at: stacks-block-height,
        active: true
      }
    )
    
    (map-set loan-bid-list
      { loan-id: loan-id }
      { lenders: (unwrap! (as-max-len? (append (get lenders bid-list) tx-sender) u10) ERR-INVALID-AMOUNT) }
    )
    
    (ok true)
  )
)

(define-public (accept-bid (loan-id uint) (chosen-lender principal))
  (let
    (
      (loan (unwrap! (map-get? loans { loan-id: loan-id }) ERR-LOAN-NOT-FOUND))
      (bid (unwrap! (map-get? loan-bids { loan-id: loan-id, lender: chosen-lender }) ERR-BID-NOT-FOUND))
      (auction-data (unwrap! (map-get? loan-auction-end { loan-id: loan-id }) ERR-LOAN-NOT-FOUND))
      (bid-list (default-to { lenders: (list) } (map-get? loan-bid-list { loan-id: loan-id })))
      (lender-funded-loans (default-to { loan-ids: (list) } (map-get? user-funded-loans { user: chosen-lender })))
    )
    (asserts! (is-eq tx-sender (get borrower loan)) ERR-NOT-BORROWER)
    (asserts! (is-eq (get status loan) u0) ERR-LOAN-ALREADY-FUNDED)
    (asserts! (>= stacks-block-height (get end-block auction-data)) ERR-BIDDING-CLOSED)
    (asserts! (get active bid) ERR-BID-NOT-FOUND)
    
    (try! (as-contract (stx-transfer? (get bid-amount bid) tx-sender (get borrower loan))))
    
    (map-set loans
      { loan-id: loan-id }
      (merge loan {
        status: u1,
        lender: (some chosen-lender),
        funded-at: (some stacks-block-height),
        interest-rate: (get interest-rate bid)
      })
    )
    
    (map-set user-funded-loans
      { user: chosen-lender }
      { loan-ids: (unwrap! (as-max-len? (append (get loan-ids lender-funded-loans) loan-id) u20) ERR-INVALID-AMOUNT) }
    )
    
    (unwrap! (refund-unsuccessful-bids loan-id chosen-lender (get lenders bid-list)) ERR-INVALID-AMOUNT)
    
    (ok true)
  )
)

(define-private (refund-unsuccessful-bids (loan-id uint) (winner principal) (lenders (list 10 principal)))
  (ok true)
)

(define-read-only (get-loan-bids (loan-id uint))
  (map-get? loan-bid-list { loan-id: loan-id })
)

(define-read-only (get-bid-details (loan-id uint) (lender principal))
  (map-get? loan-bids { loan-id: loan-id, lender: lender })
)

(define-read-only (get-auction-end (loan-id uint))
  (map-get? loan-auction-end { loan-id: loan-id })
)

;; === INSURANCE POOL SYSTEM ===
;; Community-driven insurance pool to protect lenders against defaults

;; Insurance pool constants
(define-constant ERR-INSUFFICIENT-POOL-FUNDS (err u117))
(define-constant ERR-CLAIM-ALREADY-PROCESSED (err u118))
(define-constant ERR-CLAIM-NOT-APPROVED (err u119))
(define-constant ERR-NOT-POOL-CONTRIBUTOR (err u120))
(define-constant ERR-WITHDRAWAL-TOO-LARGE (err u121))
(define-constant POOL-REWARD-PERCENTAGE u10) ;; 10% of successful loan interest goes to pool
(define-constant MIN-POOL-CONTRIBUTION u1000000) ;; 1 STX minimum
(define-constant CLAIM-VOTING-PERIOD u144) ;; 1 day for claim voting

;; Insurance pool data variables
(define-data-var total-pool-balance uint u0)
(define-data-var total-pool-contributors uint u0)
(define-data-var next-claim-id uint u1)
(define-data-var pool-reward-accumulated uint u0)

;; Pool contributor tracking
(define-map pool-contributors
  { contributor: principal }
  {
    contribution-amount: uint,
    contribution-blocks: uint,
    total-rewards-earned: uint,
    last-reward-claim: uint
  }
)

;; Insurance claims for defaulted loans
(define-map insurance-claims
  { claim-id: uint }
  {
    loan-id: uint,
    claimant: principal,
    claim-amount: uint,
    submitted-at: uint,
    status: uint, ;; 0=pending, 1=approved, 2=rejected, 3=paid
    votes-for: uint,
    votes-against: uint,
    voting-deadline: uint
  }
)

;; Claim voting tracking
(define-map claim-votes
  { claim-id: uint, voter: principal }
  { vote: bool, voting-power: uint }
)

;; Contributor list for iteration
(define-map contributor-list
  { index: uint }
  { contributor: principal }
)

(define-data-var contributor-count uint u0)

;; Contribute to insurance pool
(define-public (contribute-to-pool (amount uint))
  (let
    (
      (current-contribution (default-to 
        { contribution-amount: u0, contribution-blocks: u0, total-rewards-earned: u0, last-reward-claim: u0 }
        (map-get? pool-contributors { contributor: tx-sender })))
      (new-total-contribution (+ (get contribution-amount current-contribution) amount))
    )
    ;; Validate minimum contribution
    (asserts! (>= amount MIN-POOL-CONTRIBUTION) ERR-INVALID-AMOUNT)
    
    ;; Transfer STX to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Update pool balance
    (var-set total-pool-balance (+ (var-get total-pool-balance) amount))
    
    ;; Update or create contributor record
    (if (is-eq (get contribution-amount current-contribution) u0)
      (begin
        ;; New contributor
        (map-set contributor-list 
          { index: (var-get contributor-count) }
          { contributor: tx-sender })
        (var-set contributor-count (+ (var-get contributor-count) u1))
        (var-set total-pool-contributors (+ (var-get total-pool-contributors) u1))
      )
      true
    )
    
    (map-set pool-contributors
      { contributor: tx-sender }
      {
        contribution-amount: new-total-contribution,
        contribution-blocks: stacks-block-height,
        total-rewards-earned: (get total-rewards-earned current-contribution),
        last-reward-claim: (get last-reward-claim current-contribution)
      }
    )
    
    (ok true)
  )
)

;; Withdraw from insurance pool (partial withdrawals allowed)
(define-public (withdraw-from-pool (amount uint))
  (let
    (
      (contributor-data (unwrap! (map-get? pool-contributors { contributor: tx-sender }) ERR-NOT-POOL-CONTRIBUTOR))
      (available-amount (get contribution-amount contributor-data))
    )
    ;; Check withdrawal amount
    (asserts! (<= amount available-amount) ERR-WITHDRAWAL-TOO-LARGE)
    (asserts! (>= (var-get total-pool-balance) amount) ERR-INSUFFICIENT-POOL-FUNDS)
    
    ;; Transfer STX back to contributor
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
    
    ;; Update balances
    (var-set total-pool-balance (- (var-get total-pool-balance) amount))
    
    (let ((new-contribution-amount (- available-amount amount)))
      (if (is-eq new-contribution-amount u0)
        ;; Remove contributor if zero contribution
        (begin
          (map-delete pool-contributors { contributor: tx-sender })
          (var-set total-pool-contributors (- (var-get total-pool-contributors) u1))
        )
        ;; Update contribution amount
        (map-set pool-contributors
          { contributor: tx-sender }
          (merge contributor-data { contribution-amount: new-contribution-amount })
        )
      )
    )
    
    (ok true)
  )
)

;; Submit insurance claim for defaulted loan
(define-public (submit-insurance-claim (loan-id uint))
  (let
    (
      (loan (unwrap! (map-get? loans { loan-id: loan-id }) ERR-LOAN-NOT-FOUND))
      (lender (unwrap! (get lender loan) ERR-LOAN-NOT-FUNDED))
      (claim-id (var-get next-claim-id))
      (claim-amount (get amount loan))
    )
    ;; Verify caller is the lender
    (asserts! (is-eq tx-sender lender) ERR-NOT-LENDER)
    ;; Verify loan is defaulted
    (asserts! (is-eq (get status loan) u3) ERR-LOAN-NOT-DEFAULTED)
    ;; Check if pool has sufficient funds
    (asserts! (>= (var-get total-pool-balance) claim-amount) ERR-INSUFFICIENT-POOL-FUNDS)
    
    ;; Create insurance claim
    (map-set insurance-claims
      { claim-id: claim-id }
      {
        loan-id: loan-id,
        claimant: lender,
        claim-amount: claim-amount,
        submitted-at: stacks-block-height,
        status: u0, ;; Pending
        votes-for: u0,
        votes-against: u0,
        voting-deadline: (+ stacks-block-height CLAIM-VOTING-PERIOD)
      }
    )
    
    (var-set next-claim-id (+ claim-id u1))
    (ok claim-id)
  )
)

;; Vote on insurance claim (pool contributors only)
(define-public (vote-on-claim (claim-id uint) (approve bool))
  (let
    (
      (claim (unwrap! (map-get? insurance-claims { claim-id: claim-id }) ERR-LOAN-NOT-FOUND))
      (contributor (unwrap! (map-get? pool-contributors { contributor: tx-sender }) ERR-NOT-POOL-CONTRIBUTOR))
      (voting-power (get contribution-amount contributor))
    )
    ;; Check voting deadline
    (asserts! (< stacks-block-height (get voting-deadline claim)) ERR-INVALID-AMOUNT)
    ;; Check claim is still pending
    (asserts! (is-eq (get status claim) u0) ERR-CLAIM-ALREADY-PROCESSED)
    
    ;; Record vote
    (map-set claim-votes
      { claim-id: claim-id, voter: tx-sender }
      { vote: approve, voting-power: voting-power }
    )
    
    ;; Update claim vote counts
    (if approve
      (map-set insurance-claims
        { claim-id: claim-id }
        (merge claim { votes-for: (+ (get votes-for claim) voting-power) })
      )
      (map-set insurance-claims
        { claim-id: claim-id }
        (merge claim { votes-against: (+ (get votes-against claim) voting-power) })
      )
    )
    
    (ok true)
  )
)

;; Process insurance claim after voting period
(define-public (process-insurance-claim (claim-id uint))
  (let
    (
      (claim (unwrap! (map-get? insurance-claims { claim-id: claim-id }) ERR-LOAN-NOT-FOUND))
      (total-votes (+ (get votes-for claim) (get votes-against claim)))
      (claim-approved (> (get votes-for claim) (get votes-against claim)))
    )
    ;; Check voting deadline has passed
    (asserts! (>= stacks-block-height (get voting-deadline claim)) ERR-INVALID-AMOUNT)
    ;; Check claim is still pending
    (asserts! (is-eq (get status claim) u0) ERR-CLAIM-ALREADY-PROCESSED)
    
    (if claim-approved
      ;; Approve and pay claim
      (begin
        (try! (as-contract (stx-transfer? (get claim-amount claim) tx-sender (get claimant claim))))
        (var-set total-pool-balance (- (var-get total-pool-balance) (get claim-amount claim)))
        (map-set insurance-claims
          { claim-id: claim-id }
          (merge claim { status: u3 }) ;; Paid
        )
      )
      ;; Reject claim
      (map-set insurance-claims
        { claim-id: claim-id }
        (merge claim { status: u2 }) ;; Rejected
      )
    )
    
    (ok claim-approved)
  )
)

;; Distribute rewards to pool contributors from successful loan interest
(define-public (distribute-pool-rewards (loan-id uint))
  (let
    (
      (loan (unwrap! (map-get? loans { loan-id: loan-id }) ERR-LOAN-NOT-FOUND))
      (interest-amount (/ (* (get amount loan) (get interest-rate loan)) u10000))
      (pool-reward (/ (* interest-amount POOL-REWARD-PERCENTAGE) u100))
    )
    ;; Verify loan was successfully repaid
    (asserts! (is-eq (get status loan) u2) ERR-LOAN-ALREADY-REPAID)
    ;; Only contract owner can distribute rewards
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    
    ;; Add to accumulated pool rewards
    (var-set pool-reward-accumulated (+ (var-get pool-reward-accumulated) pool-reward))
    
    (ok true)
  )
)

;; Claim accumulated pool rewards (proportional to contribution)
(define-public (claim-pool-rewards)
  (let
    (
      (contributor (unwrap! (map-get? pool-contributors { contributor: tx-sender }) ERR-NOT-POOL-CONTRIBUTOR))
      (total-rewards (var-get pool-reward-accumulated))
      (contributor-share (/ (* total-rewards (get contribution-amount contributor)) (var-get total-pool-balance)))
    )
    ;; Check if there are rewards to distribute
    (asserts! (> contributor-share u0) ERR-INVALID-AMOUNT)
    
    ;; Transfer reward share
    (try! (as-contract (stx-transfer? contributor-share tx-sender tx-sender)))
    
    ;; Update contributor's reward tracking
    (map-set pool-contributors
      { contributor: tx-sender }
      (merge contributor {
        total-rewards-earned: (+ (get total-rewards-earned contributor) contributor-share),
        last-reward-claim: stacks-block-height
      })
    )
    
    ;; Reduce accumulated rewards
    (var-set pool-reward-accumulated (- total-rewards contributor-share))
    
    (ok contributor-share)
  )
)

;; Read-only functions for insurance pool

(define-read-only (get-pool-stats)
  {
    total-balance: (var-get total-pool-balance),
    total-contributors: (var-get total-pool-contributors),
    accumulated-rewards: (var-get pool-reward-accumulated)
  }
)

(define-read-only (get-contributor-info (contributor principal))
  (map-get? pool-contributors { contributor: contributor })
)

(define-read-only (get-insurance-claim (claim-id uint))
  (map-get? insurance-claims { claim-id: claim-id })
)

(define-read-only (get-claim-vote (claim-id uint) (voter principal))
  (map-get? claim-votes { claim-id: claim-id, voter: voter })
)

(define-read-only (get-lowest-bid (loan-id uint))
  (let
    (
      (bid-list (default-to { lenders: (list) } (map-get? loan-bid-list { loan-id: loan-id })))
      (result (fold find-lowest-bid-helper (get lenders bid-list) { loan-id: loan-id, current-lowest: none }))
    )
    (get current-lowest result)
  )
)

(define-private (find-lowest-bid-helper (lender principal) (acc { loan-id: uint, current-lowest: (optional { lender: principal, rate: uint }) }))
  (let
    (
      (loan-id (get loan-id acc))
      (current-lowest (get current-lowest acc))
      (bid (map-get? loan-bids { loan-id: loan-id, lender: lender }))
    )
    (match bid
      bid-data (if (get active bid-data)
        (match current-lowest
          current (if (< (get interest-rate bid-data) (get rate current))
            { loan-id: loan-id, current-lowest: (some { lender: lender, rate: (get interest-rate bid-data) }) }
            acc
          )
          { loan-id: loan-id, current-lowest: (some { lender: lender, rate: (get interest-rate bid-data) }) }
        )
        acc
      )
      acc
    )
  )
)
