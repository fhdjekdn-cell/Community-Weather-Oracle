;; Hyper-local Data Marketplace Smart Contract
;; Facilitates monetization of hyper-local weather data for agriculture, insurance, and logistics

;; Contract Constants
(define-constant contract-owner tx-sender)
(define-constant err-unauthorized (err u300))
(define-constant err-invalid-listing (err u301))
(define-constant err-listing-exists (err u302))
(define-constant err-insufficient-payment (err u303))
(define-constant err-purchase-exists (err u304))
(define-constant err-invalid-coordinates (err u305))
(define-constant err-invalid-price (err u306))
(define-constant err-listing-inactive (err u307))
(define-constant err-invalid-timeframe (err u308))
(define-constant err-access-expired (err u309))
(define-constant err-no-data-available (err u310))

;; Marketplace parameters
(define-constant platform-fee-percentage u5) ;; 5% platform fee
(define-constant min-listing-price u100) ;; Minimum price per data point
(define-constant max-listing-duration u2592000) ;; 30 days in seconds
(define-constant min-listing-duration u86400) ;; 1 day in seconds
(define-constant data-access-duration u604800) ;; 7 days access after purchase

;; Data Maps
(define-map data-providers
  { provider: principal }
  {
    total-listings: uint,
    active-listings: uint,
    total-revenue: uint,
    reputation-score: uint, ;; 0-100 based on buyer feedback
    registration-height: uint,
    verified: bool
  }
)

(define-map data-listings
  { listing-id: (string-ascii 64) }
  {
    provider: principal,
    title: (string-ascii 128),
    description: (string-ascii 256),
    location-lat: int, ;; scaled by 1e6
    location-lng: int, ;; scaled by 1e6
    radius: uint, ;; coverage radius in meters
    data-types: (string-ascii 128), ;; comma-separated: temp,humidity,pressure,etc
    price-per-hour: uint, ;; micro-tokens
    price-per-day: uint, ;; micro-tokens
    price-per-week: uint, ;; micro-tokens
    created-at: uint,
    expires-at: uint,
    active: bool,
    total-purchases: uint,
    last-updated: uint
  }
)

(define-map data-purchases
  { purchase-id: (string-ascii 64) }
  {
    listing-id: (string-ascii 64),
    buyer: principal,
    provider: principal,
    purchase-time: uint,
    access-expires: uint,
    duration-type: (string-ascii 16), ;; "hour", "day", "week"
    amount-paid: uint,
    platform-fee: uint,
    provider-earnings: uint,
    data-delivered: bool,
    buyer-rating: uint ;; 1-5 stars, 0 = not rated
  }
)

(define-map access-permissions
  { buyer: principal, listing-id: (string-ascii 64) }
  {
    purchase-id: (string-ascii 64),
    access-granted: uint,
    access-expires: uint,
    data-requests: uint
  }
)

(define-map data-requests
  { request-id: (string-ascii 64) }
  {
    purchase-id: (string-ascii 64),
    requester: principal,
    listing-id: (string-ascii 64),
    request-time: uint,
    from-time: uint, ;; requested data time range start
    to-time: uint, ;; requested data time range end
    fulfilled: bool
  }
)

;; Contract Variables
(define-data-var listings-counter uint u0)
(define-data-var purchases-counter uint u0)
(define-data-var requests-counter uint u0)
(define-data-var total-platform-revenue uint u0)
(define-data-var contract-active bool true)

;; Public Functions

;; Register as a data provider
(define-public (register-provider)
  (begin
    (asserts! (var-get contract-active) err-unauthorized)
    (asserts! (is-none (map-get? data-providers { provider: tx-sender })) err-listing-exists)
    
    (map-set data-providers
      { provider: tx-sender }
      {
        total-listings: u0,
        active-listings: u0,
        total-revenue: u0,
        reputation-score: u100, ;; Start with perfect score
        registration-height: stacks-block-height,
        verified: false
      }
    )
    (ok true)
  )
)

;; Create data listing
(define-public (create-listing
    (listing-id (string-ascii 64))
    (title (string-ascii 128))
    (description (string-ascii 256))
    (lat int)
    (lng int)
    (radius uint)
    (data-types (string-ascii 128))
    (price-hour uint)
    (price-day uint)
    (price-week uint)
    (duration uint))
  (let
    (
      (provider-data (unwrap! (map-get? data-providers { provider: tx-sender }) err-unauthorized))
      (current-time (unwrap-panic (get-stacks-block-info? time stacks-block-height)))
      (expires-at (+ current-time duration))
    )
    (begin
      (asserts! (var-get contract-active) err-unauthorized)
      (asserts! (is-none (map-get? data-listings { listing-id: listing-id })) err-listing-exists)
      
      ;; Validate inputs
      (asserts! (and (>= lat -90000000) (<= lat 90000000)) err-invalid-coordinates)
      (asserts! (and (>= lng -180000000) (<= lng 180000000)) err-invalid-coordinates)
      (asserts! (and (>= radius u100) (<= radius u100000)) err-invalid-coordinates) ;; 100m to 100km
      (asserts! (and (>= price-hour min-listing-price) (>= price-day min-listing-price) (>= price-week min-listing-price)) err-invalid-price)
      (asserts! (and (>= duration min-listing-duration) (<= duration max-listing-duration)) err-invalid-timeframe)
      
      ;; Create listing
      (map-set data-listings
        { listing-id: listing-id }
        {
          provider: tx-sender,
          title: title,
          description: description,
          location-lat: lat,
          location-lng: lng,
          radius: radius,
          data-types: data-types,
          price-per-hour: price-hour,
          price-per-day: price-day,
          price-per-week: price-week,
          created-at: current-time,
          expires-at: expires-at,
          active: true,
          total-purchases: u0,
          last-updated: current-time
        }
      )
      
      ;; Update provider stats
      (map-set data-providers
        { provider: tx-sender }
        (merge provider-data {
          total-listings: (+ (get total-listings provider-data) u1),
          active-listings: (+ (get active-listings provider-data) u1)
        })
      )
      
      (var-set listings-counter (+ (var-get listings-counter) u1))
      (ok listing-id)
    )
  )
)

;; Purchase data access
(define-public (purchase-data-access
    (purchase-id (string-ascii 64))
    (listing-id (string-ascii 64))
    (duration-type (string-ascii 16))
    (payment uint))
  (let
    (
      (listing-data (unwrap! (map-get? data-listings { listing-id: listing-id }) err-invalid-listing))
      (current-time (unwrap-panic (get-stacks-block-info? time stacks-block-height)))
      (required-price (get-price-by-duration listing-data duration-type))
      (platform-fee (/ (* required-price platform-fee-percentage) u100))
      (provider-earnings (- required-price platform-fee))
      (access-duration (get-access-duration duration-type))
    )
    (begin
      (asserts! (var-get contract-active) err-unauthorized)
      (asserts! (is-none (map-get? data-purchases { purchase-id: purchase-id })) err-purchase-exists)
      (asserts! (get active listing-data) err-listing-inactive)
      (asserts! (> (get expires-at listing-data) current-time) err-listing-inactive)
      (asserts! (>= payment required-price) err-insufficient-payment)
      
      ;; Record purchase
      (map-set data-purchases
        { purchase-id: purchase-id }
        {
          listing-id: listing-id,
          buyer: tx-sender,
          provider: (get provider listing-data),
          purchase-time: current-time,
          access-expires: (+ current-time access-duration),
          duration-type: duration-type,
          amount-paid: payment,
          platform-fee: platform-fee,
          provider-earnings: provider-earnings,
          data-delivered: false,
          buyer-rating: u0
        }
      )
      
      ;; Grant access permissions
      (map-set access-permissions
        { buyer: tx-sender, listing-id: listing-id }
        {
          purchase-id: purchase-id,
          access-granted: current-time,
          access-expires: (+ current-time access-duration),
          data-requests: u0
        }
      )
      
      ;; Update listing stats
      (map-set data-listings
        { listing-id: listing-id }
        (merge listing-data {
          total-purchases: (+ (get total-purchases listing-data) u1),
          last-updated: current-time
        })
      )
      
      ;; Update provider earnings
      (let
        (
          (provider-data (unwrap-panic (map-get? data-providers { provider: (get provider listing-data) })))
        )
        (map-set data-providers
          { provider: (get provider listing-data) }
          (merge provider-data {
            total-revenue: (+ (get total-revenue provider-data) provider-earnings)
          })
        )
      )
      
      ;; Update platform revenue
      (var-set total-platform-revenue (+ (var-get total-platform-revenue) platform-fee))
      (var-set purchases-counter (+ (var-get purchases-counter) u1))
      
      (ok purchase-id)
    )
  )
)

;; Request specific data from purchased access
(define-public (request-data
    (request-id (string-ascii 64))
    (listing-id (string-ascii 64))
    (from-time uint)
    (to-time uint))
  (let
    (
      (access-data (unwrap! (map-get? access-permissions { buyer: tx-sender, listing-id: listing-id }) err-unauthorized))
      (current-time (unwrap-panic (get-stacks-block-info? time stacks-block-height)))
    )
    (begin
      (asserts! (var-get contract-active) err-unauthorized)
      (asserts! (is-none (map-get? data-requests { request-id: request-id })) err-purchase-exists)
      (asserts! (>= (get access-expires access-data) current-time) err-access-expired)
      (asserts! (< from-time to-time) err-invalid-timeframe)
      (asserts! (<= to-time current-time) err-invalid-timeframe) ;; Can't request future data
      
      ;; Create data request
      (map-set data-requests
        { request-id: request-id }
        {
          purchase-id: (get purchase-id access-data),
          requester: tx-sender,
          listing-id: listing-id,
          request-time: current-time,
          from-time: from-time,
          to-time: to-time,
          fulfilled: false
        }
      )
      
      ;; Update access permissions
      (map-set access-permissions
        { buyer: tx-sender, listing-id: listing-id }
        (merge access-data {
          data-requests: (+ (get data-requests access-data) u1)
        })
      )
      
      (var-set requests-counter (+ (var-get requests-counter) u1))
      (ok request-id)
    )
  )
)

;; Rate data provider (buyers only)
(define-public (rate-provider (purchase-id (string-ascii 64)) (rating uint))
  (let
    (
      (purchase-data (unwrap! (map-get? data-purchases { purchase-id: purchase-id }) err-invalid-listing))
    )
    (begin
      (asserts! (var-get contract-active) err-unauthorized)
      (asserts! (is-eq tx-sender (get buyer purchase-data)) err-unauthorized)
      (asserts! (is-eq (get buyer-rating purchase-data) u0) err-purchase-exists) ;; Not already rated
      (asserts! (and (>= rating u1) (<= rating u5)) err-invalid-price)
      
      ;; Update purchase with rating
      (map-set data-purchases
        { purchase-id: purchase-id }
        (merge purchase-data { buyer-rating: rating })
      )
      
      ;; Update provider reputation (simplified average)
      (update-provider-reputation (get provider purchase-data) rating)
      
      (ok rating)
    )
  )
)

;; Deactivate listing (provider only)
(define-public (deactivate-listing (listing-id (string-ascii 64)))
  (let
    (
      (listing-data (unwrap! (map-get? data-listings { listing-id: listing-id }) err-invalid-listing))
    )
    (begin
      (asserts! (var-get contract-active) err-unauthorized)
      (asserts! (is-eq tx-sender (get provider listing-data)) err-unauthorized)
      (asserts! (get active listing-data) err-listing-inactive)
      
      ;; Deactivate listing
      (map-set data-listings
        { listing-id: listing-id }
        (merge listing-data { active: false })
      )
      
      ;; Update provider stats
      (let
        (
          (provider-data (unwrap-panic (map-get? data-providers { provider: tx-sender })))
        )
        (map-set data-providers
          { provider: tx-sender }
          (merge provider-data {
            active-listings: (- (get active-listings provider-data) u1)
          })
        )
      )
      
      (ok true)
    )
  )
)

;; Private Functions

;; Get price based on duration type
(define-private (get-price-by-duration (listing-data (tuple (provider principal) (title (string-ascii 128)) (description (string-ascii 256)) (location-lat int) (location-lng int) (radius uint) (data-types (string-ascii 128)) (price-per-hour uint) (price-per-day uint) (price-per-week uint) (created-at uint) (expires-at uint) (active bool) (total-purchases uint) (last-updated uint))) (duration-type (string-ascii 16)))
  (if (is-eq duration-type "hour")
      (get price-per-hour listing-data)
      (if (is-eq duration-type "day")
          (get price-per-day listing-data)
          (get price-per-week listing-data)
      )
  )
)

;; Get access duration in seconds
(define-private (get-access-duration (duration-type (string-ascii 16)))
  (if (is-eq duration-type "hour")
      u3600    ;; 1 hour
      (if (is-eq duration-type "day")
          u86400   ;; 1 day
          u604800  ;; 1 week
      )
  )
)

;; Update provider reputation based on ratings
(define-private (update-provider-reputation (provider principal) (new-rating uint))
  (let
    (
      (provider-data (unwrap-panic (map-get? data-providers { provider: provider })))
      ;; Simplified reputation update - in practice would use more sophisticated weighted average
      (current-score (get reputation-score provider-data))
      (updated-score (/ (+ current-score (* new-rating u20)) u2)) ;; Simple average with weighting
    )
    (map-set data-providers
      { provider: provider }
      (merge provider-data {
        reputation-score: (if (> updated-score u100) u100 updated-score)
      })
    )
  )
)

;; Read-only Functions

;; Get data provider information
(define-read-only (get-provider-info (provider principal))
  (map-get? data-providers { provider: provider })
)

;; Get listing details
(define-read-only (get-listing (listing-id (string-ascii 64)))
  (map-get? data-listings { listing-id: listing-id })
)

;; Get purchase details
(define-read-only (get-purchase (purchase-id (string-ascii 64)))
  (map-get? data-purchases { purchase-id: purchase-id })
)

;; Get access permissions
(define-read-only (get-access-permissions (buyer principal) (listing-id (string-ascii 64)))
  (map-get? access-permissions { buyer: buyer, listing-id: listing-id })
)

;; Get data request details
(define-read-only (get-data-request (request-id (string-ascii 64)))
  (map-get? data-requests { request-id: request-id })
)

;; Check if buyer has valid access
(define-read-only (has-valid-access (buyer principal) (listing-id (string-ascii 64)))
  (match (map-get? access-permissions { buyer: buyer, listing-id: listing-id })
    access-data (>= (get access-expires access-data) (unwrap-panic (get-stacks-block-info? time stacks-block-height)))
    false
  )
)

;; Get marketplace statistics
(define-read-only (get-marketplace-stats)
  {
    total-listings: (var-get listings-counter),
    total-purchases: (var-get purchases-counter),
    total-requests: (var-get requests-counter),
    platform-revenue: (var-get total-platform-revenue),
    active: (var-get contract-active)
  }
)

;; Get active listings in area (simplified - returns boolean for demo)
(define-read-only (has-listings-in-area (lat int) (lng int) (search-radius uint))
  (and
    (and (>= lat -90000000) (<= lat 90000000))
    (and (>= lng -180000000) (<= lng 180000000))
    (> search-radius u0)
  )
)

;; Calculate distance between two points (simplified version)
(define-read-only (calculate-distance (lat1 int) (lng1 int) (lat2 int) (lng2 int))
  (let
    (
      (lat-diff (if (> lat1 lat2) (- lat1 lat2) (- lat2 lat1)))
      (lng-diff (if (> lng1 lng2) (- lng1 lng2) (- lng2 lng1)))
    )
    ;; Simplified distance calculation (not actual geographic distance)
    (+ (to-uint lat-diff) (to-uint lng-diff))
  )
)

;; title: hyper-local-data-marketplace
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

