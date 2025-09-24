;; Weather Sensor Incentives Smart Contract
;; Provides token rewards for users submitting accurate weather data through verified sensors

;; Contract Constants
(define-constant contract-owner tx-sender)
(define-constant err-unauthorized (err u100))
(define-constant err-invalid-sensor (err u101))
(define-constant err-invalid-observation (err u102))
(define-constant err-sensor-exists (err u103))
(define-constant err-insufficient-reward-pool (err u104))
(define-constant err-observation-too-recent (err u105))
(define-constant err-invalid-coordinates (err u106))
(define-constant err-sensor-inactive (err u107))
(define-constant err-reward-already-claimed (err u108))

;; Token rewards per valid observation
(define-constant base-reward u1000) ;; micro-tokens
(define-constant quality-bonus u500) ;; additional reward for high-quality data
(define-constant min-observation-interval u3600) ;; 1 hour in seconds

;; Data Maps
(define-map sensors
  { sensor-id: (string-ascii 64) }
  {
    owner: principal,
    location-lat: int, ;; scaled by 1e6 for precision
    location-lng: int, ;; scaled by 1e6 for precision
    registration-height: uint,
    active: bool,
    total-observations: uint,
    last-observation-time: uint
  }
)

(define-map observations
  { observation-id: (string-ascii 64) }
  {
    sensor-id: (string-ascii 64),
    contributor: principal,
    timestamp: uint,
    temperature: int, ;; celsius * 100 for precision
    humidity: uint,   ;; percentage * 100
    pressure: uint,   ;; hPa * 100
    precipitation: uint, ;; mm * 100
    wind-speed: uint, ;; m/s * 100
    quality-score: uint, ;; 0-100
    reward-claimed: bool
  }
)

(define-map contributor-stats
  { contributor: principal }
  {
    total-observations: uint,
    total-rewards-earned: uint,
    last-observation-height: uint,
    active-sensors: uint
  }
)

;; Contract Variables
(define-data-var total-reward-pool uint u0)
(define-data-var observations-counter uint u0)
(define-data-var sensors-counter uint u0)
(define-data-var contract-active bool true)

;; Public Functions

;; Register a new weather sensor
(define-public (register-sensor (sensor-id (string-ascii 64)) (lat int) (lng int))
  (begin
    (asserts! (var-get contract-active) err-unauthorized)
    (asserts! (is-none (map-get? sensors { sensor-id: sensor-id })) err-sensor-exists)
    (asserts! (and (>= lat -90000000) (<= lat 90000000)) err-invalid-coordinates)
    (asserts! (and (>= lng -180000000) (<= lng 180000000)) err-invalid-coordinates)
    
    (map-set sensors
      { sensor-id: sensor-id }
      {
        owner: tx-sender,
        location-lat: lat,
        location-lng: lng,
        registration-height: stacks-block-height,
        active: true,
        total-observations: u0,
        last-observation-time: u0
      }
    )
    
    ;; Update contributor stats
    (map-set contributor-stats
      { contributor: tx-sender }
      (merge
        (default-to
          { total-observations: u0, total-rewards-earned: u0, last-observation-height: u0, active-sensors: u0 }
          (map-get? contributor-stats { contributor: tx-sender })
        )
        { active-sensors: (+ (get active-sensors 
                               (default-to { active-sensors: u0 } 
                                         (map-get? contributor-stats { contributor: tx-sender }))) u1) }
      )
    )
    
    (var-set sensors-counter (+ (var-get sensors-counter) u1))
    (ok sensor-id)
  )
)

;; Submit weather observation data
(define-public (submit-observation 
    (sensor-id (string-ascii 64))
    (observation-id (string-ascii 64))
    (temperature int)
    (humidity uint)
    (pressure uint)
    (precipitation uint)
    (wind-speed uint)
    (quality-score uint))
  (let
    (
      (sensor-data (unwrap! (map-get? sensors { sensor-id: sensor-id }) err-invalid-sensor))
      (current-time (unwrap-panic (get-stacks-block-info? time stacks-block-height)))
    )
    (begin
      (asserts! (var-get contract-active) err-unauthorized)
      (asserts! (is-eq (get owner sensor-data) tx-sender) err-unauthorized)
      (asserts! (get active sensor-data) err-sensor-inactive)
      (asserts! (is-none (map-get? observations { observation-id: observation-id })) err-invalid-observation)
      
      ;; Validate observation data ranges
      (asserts! (and (>= temperature -5000) (<= temperature 6000)) err-invalid-observation) ;; -50C to 60C
      (asserts! (and (>= humidity u0) (<= humidity u10000)) err-invalid-observation) ;; 0-100%
      (asserts! (and (>= pressure u80000) (<= pressure u108000)) err-invalid-observation) ;; 800-1080 hPa
      (asserts! (and (>= precipitation u0) (<= precipitation u50000)) err-invalid-observation) ;; 0-500mm
      (asserts! (and (>= wind-speed u0) (<= wind-speed u7000)) err-invalid-observation) ;; 0-70 m/s
      (asserts! (<= quality-score u100) err-invalid-observation)
      
      ;; Check minimum time interval between observations
      (asserts! (> (- current-time (get last-observation-time sensor-data)) min-observation-interval) err-observation-too-recent)
      
      ;; Store observation
      (map-set observations
        { observation-id: observation-id }
        {
          sensor-id: sensor-id,
          contributor: tx-sender,
          timestamp: current-time,
          temperature: temperature,
          humidity: humidity,
          pressure: pressure,
          precipitation: precipitation,
          wind-speed: wind-speed,
          quality-score: quality-score,
          reward-claimed: false
        }
      )
      
      ;; Update sensor stats
      (map-set sensors
        { sensor-id: sensor-id }
        (merge sensor-data {
          total-observations: (+ (get total-observations sensor-data) u1),
          last-observation-time: current-time
        })
      )
      
      ;; Update contributor stats
      (let
        (
          (contributor-data (default-to
            { total-observations: u0, total-rewards-earned: u0, last-observation-height: u0, active-sensors: u0 }
            (map-get? contributor-stats { contributor: tx-sender })
          ))
        )
        (map-set contributor-stats
          { contributor: tx-sender }
          (merge contributor-data {
            total-observations: (+ (get total-observations contributor-data) u1),
            last-observation-height: stacks-block-height
          })
        )
      )
      
      (var-set observations-counter (+ (var-get observations-counter) u1))
      (ok observation-id)
    )
  )
)

;; Claim reward for a submitted observation
(define-public (claim-observation-reward (observation-id (string-ascii 64)))
  (let
    (
      (observation-data (unwrap! (map-get? observations { observation-id: observation-id }) err-invalid-observation))
      (reward-amount (calculate-reward (get quality-score observation-data)))
    )
    (begin
      (asserts! (var-get contract-active) err-unauthorized)
      (asserts! (is-eq (get contributor observation-data) tx-sender) err-unauthorized)
      (asserts! (not (get reward-claimed observation-data)) err-reward-already-claimed)
      (asserts! (>= (var-get total-reward-pool) reward-amount) err-insufficient-reward-pool)
      
      ;; Mark reward as claimed
      (map-set observations
        { observation-id: observation-id }
        (merge observation-data { reward-claimed: true })
      )
      
      ;; Deduct from reward pool and update contributor stats
      (var-set total-reward-pool (- (var-get total-reward-pool) reward-amount))
      
      (let
        (
          (contributor-data (unwrap-panic (map-get? contributor-stats { contributor: tx-sender })))
        )
        (map-set contributor-stats
          { contributor: tx-sender }
          (merge contributor-data {
            total-rewards-earned: (+ (get total-rewards-earned contributor-data) reward-amount)
          })
        )
      )
      
      (ok reward-amount)
    )
  )
)

;; Add funds to the reward pool (owner only)
(define-public (fund-reward-pool (amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    (var-set total-reward-pool (+ (var-get total-reward-pool) amount))
    (ok amount)
  )
)

;; Deactivate a sensor (owner only)
(define-public (deactivate-sensor (sensor-id (string-ascii 64)))
  (let
    (
      (sensor-data (unwrap! (map-get? sensors { sensor-id: sensor-id }) err-invalid-sensor))
    )
    (begin
      (asserts! (is-eq tx-sender (get owner sensor-data)) err-unauthorized)
      (map-set sensors
        { sensor-id: sensor-id }
        (merge sensor-data { active: false })
      )
      (ok true)
    )
  )
)

;; Private Functions

;; Calculate reward based on quality score
(define-private (calculate-reward (quality-score uint))
  (if (>= quality-score u80)
      (+ base-reward quality-bonus)
      base-reward
  )
)

;; Read-only Functions

;; Get sensor information
(define-read-only (get-sensor-info (sensor-id (string-ascii 64)))
  (map-get? sensors { sensor-id: sensor-id })
)

;; Get observation data
(define-read-only (get-observation (observation-id (string-ascii 64)))
  (map-get? observations { observation-id: observation-id })
)

;; Get contributor statistics
(define-read-only (get-contributor-stats (contributor principal))
  (map-get? contributor-stats { contributor: contributor })
)

;; Get total reward pool
(define-read-only (get-reward-pool-balance)
  (var-get total-reward-pool)
)

;; Get contract statistics
(define-read-only (get-contract-stats)
  {
    total-sensors: (var-get sensors-counter),
    total-observations: (var-get observations-counter),
    reward-pool: (var-get total-reward-pool),
    active: (var-get contract-active)
  }
)

;; Check if sensor is active and belongs to caller
(define-read-only (is-sensor-owner (sensor-id (string-ascii 64)) (caller principal))
  (match (map-get? sensors { sensor-id: sensor-id })
    sensor-data (and (is-eq (get owner sensor-data) caller) (get active sensor-data))
    false
  )
)

;; title: weather-sensor-incentives
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

