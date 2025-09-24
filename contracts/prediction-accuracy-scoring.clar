;; Prediction Accuracy Scoring Smart Contract
;; Maintains reputation scores for weather data contributors based on accuracy

;; Contract Constants
(define-constant contract-owner tx-sender)
(define-constant err-unauthorized (err u200))
(define-constant err-invalid-contributor (err u201))
(define-constant err-invalid-prediction (err u202))
(define-constant err-prediction-exists (err u203))
(define-constant err-evaluation-exists (err u204))
(define-constant err-evaluation-too-early (err u205))
(define-constant err-invalid-reference (err u206))
(define-constant err-no-prediction-data (err u207))
(define-constant err-insufficient-stake (err u208))
(define-constant err-cooldown-active (err u209))

;; Accuracy scoring parameters
(define-constant max-accuracy-score u1000) ;; Maximum accuracy score
(define-constant min-evaluation-delay u7200) ;; 2 hours minimum before evaluation
(define-constant accuracy-window u86400) ;; 24 hours rolling window for accuracy
(define-constant stake-multiplier u100) ;; Multiplier for stake-based weighting
(define-constant min-stake-amount u1000) ;; Minimum stake required
(define-constant cooldown-period u3600) ;; 1 hour cooldown between predictions

;; Data Maps
(define-map contributors
  { contributor: principal }
  {
    total-predictions: uint,
    accurate-predictions: uint,
    current-accuracy-score: uint,
    total-stake: uint,
    last-prediction-time: uint,
    reputation-level: uint, ;; 1-5 levels based on performance
    evaluation-count: uint
  }
)

(define-map predictions
  { prediction-id: (string-ascii 64) }
  {
    contributor: principal,
    timestamp: uint,
    target-time: uint, ;; when the prediction is for
    location-lat: int, ;; scaled by 1e6
    location-lng: int, ;; scaled by 1e6
    predicted-temperature: int, ;; celsius * 100
    predicted-humidity: uint, ;; percentage * 100
    predicted-pressure: uint, ;; hPa * 100
    predicted-precipitation: uint, ;; mm * 100
    confidence-level: uint, ;; 1-100
    stake-amount: uint,
    evaluated: bool
  }
)

(define-map evaluations
  { prediction-id: (string-ascii 64) }
  {
    evaluator: principal,
    evaluation-time: uint,
    actual-temperature: int,
    actual-humidity: uint,
    actual-pressure: uint,
    actual-precipitation: uint,
    accuracy-score: uint, ;; 0-1000
    temperature-error: uint,
    humidity-error: uint,
    pressure-error: uint,
    precipitation-error: uint
  }
)

(define-map accuracy-history
  { contributor: principal, period: uint } ;; period = block-height / 144 (daily periods)
  {
    predictions-count: uint,
    total-accuracy: uint,
    average-confidence: uint,
    stake-weighted-accuracy: uint
  }
)

;; Contract Variables
(define-data-var predictions-counter uint u0)
(define-data-var evaluations-counter uint u0)
(define-data-var total-stake-pool uint u0)
(define-data-var contract-active bool true)

;; Public Functions

;; Submit weather prediction
(define-public (submit-prediction
    (prediction-id (string-ascii 64))
    (target-time uint)
    (lat int)
    (lng int)
    (temperature int)
    (humidity uint)
    (pressure uint)
    (precipitation uint)
    (confidence uint)
    (stake uint))
  (let
    (
      (current-time (unwrap-panic (get-stacks-block-info? time stacks-block-height)))
      (contributor-data (default-to
        { total-predictions: u0, accurate-predictions: u0, current-accuracy-score: u500, 
          total-stake: u0, last-prediction-time: u0, reputation-level: u1, evaluation-count: u0 }
        (map-get? contributors { contributor: tx-sender })
      ))
    )
    (begin
      (asserts! (var-get contract-active) err-unauthorized)
      (asserts! (is-none (map-get? predictions { prediction-id: prediction-id })) err-prediction-exists)
      (asserts! (>= stake min-stake-amount) err-insufficient-stake)
      (asserts! (> target-time (+ current-time min-evaluation-delay)) err-evaluation-too-early)
      (asserts! (> (- current-time (get last-prediction-time contributor-data)) cooldown-period) err-cooldown-active)
      
      ;; Validate coordinates
      (asserts! (and (>= lat -90000000) (<= lat 90000000)) err-invalid-prediction)
      (asserts! (and (>= lng -180000000) (<= lng 180000000)) err-invalid-prediction)
      
      ;; Validate prediction ranges
      (asserts! (and (>= temperature -5000) (<= temperature 6000)) err-invalid-prediction)
      (asserts! (and (>= humidity u0) (<= humidity u10000)) err-invalid-prediction)
      (asserts! (and (>= pressure u80000) (<= pressure u108000)) err-invalid-prediction)
      (asserts! (and (>= precipitation u0) (<= precipitation u50000)) err-invalid-prediction)
      (asserts! (and (>= confidence u1) (<= confidence u100)) err-invalid-prediction)
      
      ;; Store prediction
      (map-set predictions
        { prediction-id: prediction-id }
        {
          contributor: tx-sender,
          timestamp: current-time,
          target-time: target-time,
          location-lat: lat,
          location-lng: lng,
          predicted-temperature: temperature,
          predicted-humidity: humidity,
          predicted-pressure: pressure,
          predicted-precipitation: precipitation,
          confidence-level: confidence,
          stake-amount: stake,
          evaluated: false
        }
      )
      
      ;; Update contributor stats
      (map-set contributors
        { contributor: tx-sender }
        (merge contributor-data {
          total-predictions: (+ (get total-predictions contributor-data) u1),
          total-stake: (+ (get total-stake contributor-data) stake),
          last-prediction-time: current-time
        })
      )
      
      (var-set predictions-counter (+ (var-get predictions-counter) u1))
      (var-set total-stake-pool (+ (var-get total-stake-pool) stake))
      (ok prediction-id)
    )
  )
)

;; Evaluate prediction accuracy
(define-public (evaluate-prediction
    (prediction-id (string-ascii 64))
    (actual-temperature int)
    (actual-humidity uint)
    (actual-pressure uint)
    (actual-precipitation uint))
  (let
    (
      (prediction-data (unwrap! (map-get? predictions { prediction-id: prediction-id }) err-invalid-prediction))
      (current-time (unwrap-panic (get-stacks-block-info? time stacks-block-height)))
      (contributor (get contributor prediction-data))
    )
    (begin
      (asserts! (var-get contract-active) err-unauthorized)
      (asserts! (not (get evaluated prediction-data)) err-evaluation-exists)
      (asserts! (>= current-time (get target-time prediction-data)) err-evaluation-too-early)
      
      ;; Validate actual values
      (asserts! (and (>= actual-temperature -5000) (<= actual-temperature 6000)) err-invalid-reference)
      (asserts! (and (>= actual-humidity u0) (<= actual-humidity u10000)) err-invalid-reference)
      (asserts! (and (>= actual-pressure u80000) (<= actual-pressure u108000)) err-invalid-reference)
      (asserts! (and (>= actual-precipitation u0) (<= actual-precipitation u50000)) err-invalid-reference)
      
      (let
        (
          (temp-error (calculate-error (get predicted-temperature prediction-data) actual-temperature u100))
          (humidity-error (calculate-error-uint (get predicted-humidity prediction-data) actual-humidity u100))
          (pressure-error (calculate-error-uint (get predicted-pressure prediction-data) actual-pressure u1000))
          (precip-error (calculate-error-uint (get predicted-precipitation prediction-data) actual-precipitation u100))
          (overall-accuracy (calculate-overall-accuracy temp-error humidity-error pressure-error precip-error))
          (contributor-data (unwrap-panic (map-get? contributors { contributor: contributor })))
        )
        (begin
          ;; Store evaluation
          (map-set evaluations
            { prediction-id: prediction-id }
            {
              evaluator: tx-sender,
              evaluation-time: current-time,
              actual-temperature: actual-temperature,
              actual-humidity: actual-humidity,
              actual-pressure: actual-pressure,
              actual-precipitation: actual-precipitation,
              accuracy-score: overall-accuracy,
              temperature-error: temp-error,
              humidity-error: humidity-error,
              pressure-error: pressure-error,
              precipitation-error: precip-error
            }
          )
          
          ;; Mark prediction as evaluated
          (map-set predictions
            { prediction-id: prediction-id }
            (merge prediction-data { evaluated: true })
          )
          
          ;; Update contributor accuracy
          (let
            (
              (is-accurate (>= overall-accuracy u700)) ;; 70% accuracy threshold
              (new-accuracy (calculate-rolling-accuracy contributor overall-accuracy))
              (new-reputation (calculate-reputation-level new-accuracy (get total-predictions contributor-data)))
            )
            (map-set contributors
              { contributor: contributor }
              (merge contributor-data {
                accurate-predictions: (if is-accurate 
                                        (+ (get accurate-predictions contributor-data) u1)
                                        (get accurate-predictions contributor-data)),
                current-accuracy-score: new-accuracy,
                reputation-level: new-reputation,
                evaluation-count: (+ (get evaluation-count contributor-data) u1)
              })
            )
          )
          
          ;; Update accuracy history
          (update-accuracy-history contributor overall-accuracy (get confidence-level prediction-data) (get stake-amount prediction-data))
          
          (var-set evaluations-counter (+ (var-get evaluations-counter) u1))
          (ok overall-accuracy)
        )
      )
    )
  )
)

;; Withdraw stake after evaluation (contributor only)
(define-public (withdraw-stake (prediction-id (string-ascii 64)))
  (let
    (
      (prediction-data (unwrap! (map-get? predictions { prediction-id: prediction-id }) err-invalid-prediction))
      (evaluation-data (unwrap! (map-get? evaluations { prediction-id: prediction-id }) err-no-prediction-data))
      (stake-amount (get stake-amount prediction-data))
    )
    (begin
      (asserts! (var-get contract-active) err-unauthorized)
      (asserts! (is-eq tx-sender (get contributor prediction-data)) err-unauthorized)
      (asserts! (get evaluated prediction-data) err-no-prediction-data)
      
      ;; Calculate stake return based on accuracy (partial forfeit for poor predictions)
      (let
        (
          (accuracy-score (get accuracy-score evaluation-data))
          (return-amount (if (>= accuracy-score u500)
                            stake-amount
                            (/ (* stake-amount accuracy-score) u1000)))
        )
        (var-set total-stake-pool (- (var-get total-stake-pool) return-amount))
        (ok return-amount)
      )
    )
  )
)

;; Private Functions

;; Calculate absolute error between predicted and actual values
(define-private (calculate-error (predicted int) (actual int) (scale uint))
  (let
    (
      (diff (if (> predicted actual) (- predicted actual) (- actual predicted)))
    )
    (/ (* (to-uint diff) scale) scale)
  )
)

(define-private (calculate-error-uint (predicted uint) (actual uint) (scale uint))
  (let
    (
      (diff (if (> predicted actual) (- predicted actual) (- actual predicted)))
    )
    (/ (* diff scale) scale)
  )
)

;; Calculate overall accuracy score from individual errors
(define-private (calculate-overall-accuracy (temp-err uint) (humidity-err uint) (pressure-err uint) (precip-err uint))
  (let
    (
      (max-temp-err u1000) ;; 10C
      (max-humidity-err u2000) ;; 20%
      (max-pressure-err u5000) ;; 50 hPa
      (max-precip-err u1000) ;; 10mm
      
      (temp-score (if (>= temp-err max-temp-err) u0 (- u250 (/ (* temp-err u250) max-temp-err))))
      (humidity-score (if (>= humidity-err max-humidity-err) u0 (- u250 (/ (* humidity-err u250) max-humidity-err))))
      (pressure-score (if (>= pressure-err max-pressure-err) u0 (- u250 (/ (* pressure-err u250) max-pressure-err))))
      (precip-score (if (>= precip-err max-precip-err) u0 (- u250 (/ (* precip-err u250) max-precip-err))))
    )
    (+ temp-score humidity-score pressure-score precip-score)
  )
)

;; Calculate rolling accuracy for contributor
(define-private (calculate-rolling-accuracy (contributor principal) (latest-accuracy uint))
  (let
    (
      (contributor-data (unwrap-panic (map-get? contributors { contributor: contributor })))
      (evaluation-count (get evaluation-count contributor-data))
    )
    (if (is-eq evaluation-count u0)
        latest-accuracy
        (/ (+ (* (get current-accuracy-score contributor-data) evaluation-count) latest-accuracy) (+ evaluation-count u1))
    )
  )
)

;; Calculate reputation level based on accuracy and experience
(define-private (calculate-reputation-level (accuracy uint) (total-predictions uint))
  (if (and (>= accuracy u900) (>= total-predictions u100))
      u5 ;; Expert
      (if (and (>= accuracy u800) (>= total-predictions u50))
          u4 ;; Advanced
          (if (and (>= accuracy u700) (>= total-predictions u20))
              u3 ;; Intermediate
              (if (and (>= accuracy u600) (>= total-predictions u5))
                  u2 ;; Beginner
                  u1 ;; Novice
              )
          )
      )
  )
)

;; Update accuracy history for trending analysis
(define-private (update-accuracy-history (contributor principal) (accuracy uint) (confidence uint) (stake uint))
  (let
    (
      (current-period (/ stacks-block-height u144)) ;; Daily periods
      (existing-data (default-to
        { predictions-count: u0, total-accuracy: u0, average-confidence: u0, stake-weighted-accuracy: u0 }
        (map-get? accuracy-history { contributor: contributor, period: current-period })
      ))
    )
    (map-set accuracy-history
      { contributor: contributor, period: current-period }
      {
        predictions-count: (+ (get predictions-count existing-data) u1),
        total-accuracy: (+ (get total-accuracy existing-data) accuracy),
        average-confidence: (/ (+ (* (get average-confidence existing-data) (get predictions-count existing-data)) confidence) 
                              (+ (get predictions-count existing-data) u1)),
        stake-weighted-accuracy: (+ (get stake-weighted-accuracy existing-data) (* accuracy stake))
      }
    )
  )
)

;; Read-only Functions

;; Get contributor information
(define-read-only (get-contributor-info (contributor principal))
  (map-get? contributors { contributor: contributor })
)

;; Get prediction details
(define-read-only (get-prediction (prediction-id (string-ascii 64)))
  (map-get? predictions { prediction-id: prediction-id })
)

;; Get evaluation details
(define-read-only (get-evaluation (prediction-id (string-ascii 64)))
  (map-get? evaluations { prediction-id: prediction-id })
)

;; Get accuracy history for contributor and period
(define-read-only (get-accuracy-history (contributor principal) (period uint))
  (map-get? accuracy-history { contributor: contributor, period: period })
)

;; Get contributor accuracy percentage
(define-read-only (get-accuracy-percentage (contributor principal))
  (match (map-get? contributors { contributor: contributor })
    contributor-data 
      (if (is-eq (get total-predictions contributor-data) u0)
          u0
          (/ (* (get accurate-predictions contributor-data) u100) (get total-predictions contributor-data))
      )
    u0
  )
)

;; Get contract statistics
(define-read-only (get-contract-stats)
  {
    total-predictions: (var-get predictions-counter),
    total-evaluations: (var-get evaluations-counter),
    total-stake: (var-get total-stake-pool),
    active: (var-get contract-active)
  }
)

;; Check contributor reputation level
(define-read-only (get-reputation-level (contributor principal))
  (match (map-get? contributors { contributor: contributor })
    contributor-data (get reputation-level contributor-data)
    u1
  )
)

;; title: prediction-accuracy-scoring
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

