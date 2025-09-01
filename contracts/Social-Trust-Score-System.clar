(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_SCORE (err u101))
(define-constant ERR_SELF_ENDORSEMENT (err u102))
(define-constant ERR_ALREADY_ENDORSED (err u103))
(define-constant ERR_USER_NOT_FOUND (err u104))
(define-constant ERR_INSUFFICIENT_TRUST (err u105))
(define-constant ERR_INVALID_WEIGHT (err u106))
(define-constant ERR_COOLDOWN_ACTIVE (err u107))
(define-constant ERR_MILESTONE_NOT_REACHED (err u108))
(define-constant ERR_MILESTONE_ALREADY_CLAIMED (err u109))

(define-constant ERR_DELEGATION_EXISTS (err u110))
(define-constant ERR_DELEGATION_NOT_FOUND (err u111))
(define-constant ERR_DELEGATION_EXPIRED (err u112))
(define-constant ERR_INSUFFICIENT_DELEGATION_TRUST (err u113))
(define-constant MIN_DELEGATION_TRUST u80)
(define-constant MAX_DELEGATION_DURATION u2016)

(define-constant MIN_TRUST_SCORE u50)
(define-constant MAX_TRUST_SCORE u100)
(define-constant ENDORSEMENT_COOLDOWN u144)
(define-constant DECAY_RATE u1)
(define-constant DECAY_INTERVAL u1008)

(define-constant ERR_BADGE_NOT_FOUND (err u114))
(define-constant ERR_BADGE_ALREADY_EARNED (err u115))
(define-constant ERR_INSUFFICIENT_CATEGORY_ENDORSEMENTS (err u116))

(define-map user-profiles
  { user: principal }
  {
    trust-score: uint,
    total-endorsements: uint,
    total-reports: uint,
    last-activity: uint,
    reputation-level: uint,
    is-verified: bool
  }
)

(define-map endorsements
  { endorser: principal, endorsed: principal }
  {
    weight: uint,
    timestamp: uint,
    category: (string-ascii 20)
  }
)

(define-map user-endorsement-history
  { user: principal, target: principal }
  { last-endorsement: uint }
)

(define-map trust-requirements
  { service-id: (string-ascii 50) }
  {
    min-trust-score: uint,
    min-endorsements: uint,
    require-verification: bool,
    creator: principal
  }
)

(define-map user-service-access
  { user: principal, service-id: (string-ascii 50) }
  { granted: bool, granted-at: uint }
)

(define-map governance-votes
  { proposal-id: uint, voter: principal }
  {
    vote-weight: uint,
    vote-choice: bool,
    timestamp: uint
  }
)

(define-data-var next-proposal-id uint u1)
(define-data-var total-users uint u0)
(define-data-var contract-paused bool false)

(define-read-only (get-user-profile (user principal))
  (map-get? user-profiles { user: user })
)

(define-read-only (get-trust-score (user principal))
  (match (map-get? user-profiles { user: user })
    profile (ok (get trust-score profile))
    (err ERR_USER_NOT_FOUND)
  )
)

(define-read-only (get-endorsement (endorser principal) (endorsed principal))
  (map-get? endorsements { endorser: endorser, endorsed: endorsed })
)

(define-read-only (can-access-service (user principal) (service-id (string-ascii 50)))
  (match (map-get? trust-requirements { service-id: service-id })
    requirements
    (match (map-get? user-profiles { user: user })
      profile
      (let
        (
          (trust-score (get trust-score profile))
          (endorsement-count (get total-endorsements profile))
          (is-verified (get is-verified profile))
          (min-trust (get min-trust-score requirements))
          (min-endorsements (get min-endorsements requirements))
          (require-verification (get require-verification requirements))
        )
        (ok (and
          (>= trust-score min-trust)
          (>= endorsement-count min-endorsements)
          (or (not require-verification) is-verified)
        ))
      )
      (ok false)
    )
    (ok false)
  )
)

(define-read-only (get-voting-weight (user principal))
  (match (map-get? user-profiles { user: user })
    profile
    (let
      (
        (trust-score (get trust-score profile))
        (reputation-level (get reputation-level profile))
      )
      (ok (+ trust-score (* reputation-level u10)))
    )
    (ok u0)
  )
)

(define-read-only (is-endorsement-allowed (endorser principal) (endorsed principal))
  (let
    (
      (last-endorsement-data (map-get? user-endorsement-history 
        { user: endorser, target: endorsed }))
      (current-block stacks-block-height)
    )
    (if (is-eq endorser endorsed)
      (ok false)
      (match last-endorsement-data
        history
        (ok (> current-block (+ (get last-endorsement history) ENDORSEMENT_COOLDOWN)))
        (ok true)
      )
    )
  )
)

(define-public (initialize-user)
  (let
    (
      (user tx-sender)
      (existing-profile (map-get? user-profiles { user: user }))
    )
    (if (is-none existing-profile)
      (begin
        (map-set user-profiles
          { user: user }
          {
            trust-score: MIN_TRUST_SCORE,
            total-endorsements: u0,
            total-reports: u0,
            last-activity: stacks-block-height,
            reputation-level: u1,
            is-verified: false
          }
        )
        (var-set total-users (+ (var-get total-users) u1))
        (ok true)
      )
      (ok false)
    )
  )
)

(define-public (endorse-user (endorsed principal) (weight uint) (category (string-ascii 20)))
  (let
    (
      (endorser tx-sender)
      (endorser-profile (unwrap! (map-get? user-profiles { user: endorser }) ERR_USER_NOT_FOUND))
      (endorsed-profile (unwrap! (map-get? user-profiles { user: endorsed }) ERR_USER_NOT_FOUND))
      (endorser-trust (get trust-score endorser-profile))
      (can-endorse (unwrap! (is-endorsement-allowed endorser endorsed) ERR_UNAUTHORIZED))
    )
    (asserts! can-endorse ERR_SELF_ENDORSEMENT)
    (asserts! (>= endorser-trust MIN_TRUST_SCORE) ERR_INSUFFICIENT_TRUST)
    (asserts! (and (>= weight u1) (<= weight u10)) ERR_INVALID_WEIGHT)
    (asserts! (not (var-get contract-paused)) ERR_UNAUTHORIZED)
    
    (let
      (
        (adjusted-weight (/ (* weight endorser-trust) u100))
        (new-endorsement-count (+ (get total-endorsements endorsed-profile) u1))
        (score-boost (if (< (/ adjusted-weight u2) u5) (/ adjusted-weight u2) u5))
        (new-trust-score (if (> (+ (get trust-score endorsed-profile) score-boost) MAX_TRUST_SCORE)
          MAX_TRUST_SCORE
          (+ (get trust-score endorsed-profile) score-boost)))
        (new-reputation-level (/ new-endorsement-count u10))
      )
      (map-set endorsements
        { endorser: endorser, endorsed: endorsed }
        {
          weight: adjusted-weight,
          timestamp: stacks-block-height,
          category: category
        }
      )
      (map-set user-endorsement-history
        { user: endorser, target: endorsed }
        { last-endorsement: stacks-block-height }
      )
      (map-set user-profiles
        { user: endorsed }
        (merge endorsed-profile {
          trust-score: new-trust-score,
          total-endorsements: new-endorsement-count,
          last-activity: stacks-block-height,
          reputation-level: new-reputation-level
        })
      )
      (ok true)
    )
  )
)
(define-public (report-user (reported principal))
  (let
    (
      (reporter tx-sender)
      (reporter-profile (unwrap! (map-get? user-profiles { user: reporter }) ERR_USER_NOT_FOUND))
      (reported-profile (unwrap! (map-get? user-profiles { user: reported }) ERR_USER_NOT_FOUND))
      (reporter-trust (get trust-score reporter-profile))
    )
    (asserts! (not (is-eq reporter reported)) ERR_SELF_ENDORSEMENT)
    (asserts! (>= reporter-trust MIN_TRUST_SCORE) ERR_INSUFFICIENT_TRUST)
    
    (let
      (
        (penalty (/ reporter-trust u20))
        (new-reports (+ (get total-reports reported-profile) u1))
        (new-trust-score (if (> (get trust-score reported-profile) penalty)
          (- (get trust-score reported-profile) penalty)
          MIN_TRUST_SCORE))
      )
      (map-set user-profiles
        { user: reported }
        (merge reported-profile {
          trust-score: new-trust-score,
          total-reports: new-reports,
          last-activity: stacks-block-height
        })
      )
      (ok true)
    )
  )
)

(define-public (create-service-requirement 
  (service-id (string-ascii 50))
  (min-trust-score uint)
  (min-endorsements uint)
  (require-verification bool))
  (begin
    (asserts! (and (>= min-trust-score MIN_TRUST_SCORE) 
                   (<= min-trust-score MAX_TRUST_SCORE)) ERR_INVALID_SCORE)
    (map-set trust-requirements
      { service-id: service-id }
      {
        min-trust-score: min-trust-score,
        min-endorsements: min-endorsements,
        require-verification: require-verification,
        creator: tx-sender
      }
    )
    (ok true)
  )
)

(define-public (request-service-access (service-id (string-ascii 50)))
  (let
    (
      (user tx-sender)
      (has-access (unwrap! (can-access-service user service-id) ERR_UNAUTHORIZED))
    )
    (asserts! has-access ERR_INSUFFICIENT_TRUST)
    (map-set user-service-access
      { user: user, service-id: service-id }
      { granted: true, granted-at: stacks-block-height }
    )
    (ok true)
  )
)

(define-public (cast-governance-vote (proposal-id uint) (vote-choice bool))
  (let
    (
      (voter tx-sender)
      (voter-profile (unwrap! (map-get? user-profiles { user: voter }) ERR_USER_NOT_FOUND))
      (vote-weight (unwrap! (get-voting-weight voter) ERR_UNAUTHORIZED))
    )
    (asserts! (>= (get trust-score voter-profile) MIN_TRUST_SCORE) ERR_INSUFFICIENT_TRUST)
    (map-set governance-votes
      { proposal-id: proposal-id, voter: voter }
      {
        vote-weight: vote-weight,
        vote-choice: vote-choice,
        timestamp: stacks-block-height
      }
    )
    (ok vote-weight)
  )
)

(define-public (verify-user (user principal))
  (let
    (
      (user-profile (unwrap! (map-get? user-profiles { user: user }) ERR_USER_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set user-profiles
      { user: user }
      (merge user-profile { is-verified: true })
    )
    (ok true)
  )
)

(define-public (decay-trust-scores)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (ok true)
  )
)

(define-public (pause-contract)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set contract-paused true)
    (ok true)
  )
)

(define-public (unpause-contract)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set contract-paused false)
    (ok true)
  )
)



(define-map milestone-definitions
  { milestone-id: uint }
  {
    required-score: uint,
    reward-type: (string-ascii 20),
    reward-value: uint,
    badge-name: (string-ascii 30)
  }
)

(define-map user-milestones
  { user: principal, milestone-id: uint }
  {
    achieved: bool,
    claimed: bool,
    achieved-at: uint
  }
)

(define-data-var next-milestone-id uint u1)

(define-read-only (get-milestone (milestone-id uint))
  (map-get? milestone-definitions { milestone-id: milestone-id })
)

(define-read-only (get-user-milestone (user principal) (milestone-id uint))
  (map-get? user-milestones { user: user, milestone-id: milestone-id })
)

(define-read-only (get-milestone-rewards (user principal))
  (match (map-get? user-profiles { user: user })
    profile
    (let
      (
        (trust-score (get trust-score profile))
        (cooldown-reduction (if (>= trust-score u80) u50 u0))
        (vote-bonus (if (>= trust-score u90) u20 u0))
      )
      (ok { cooldown-reduction: cooldown-reduction, vote-bonus: vote-bonus })
    )
    (ok { cooldown-reduction: u0, vote-bonus: u0 })
  )
)

(define-public (create-milestone 
  (required-score uint) 
  (reward-type (string-ascii 20))
  (reward-value uint)
  (badge-name (string-ascii 30)))
  (let
    (
      (milestone-id (var-get next-milestone-id))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set milestone-definitions
      { milestone-id: milestone-id }
      {
        required-score: required-score,
        reward-type: reward-type,
        reward-value: reward-value,
        badge-name: badge-name
      }
    )
    (var-set next-milestone-id (+ milestone-id u1))
    (ok milestone-id)
  )
)

(define-public (claim-milestone (milestone-id uint))
  (let
    (
      (user tx-sender)
      (user-profile (unwrap! (map-get? user-profiles { user: user }) ERR_USER_NOT_FOUND))
      (milestone (unwrap! (get-milestone milestone-id) ERR_UNAUTHORIZED))
      (user-milestone (map-get? user-milestones { user: user, milestone-id: milestone-id }))
      (trust-score (get trust-score user-profile))
      (required-score (get required-score milestone))
    )
    (asserts! (>= trust-score required-score) ERR_MILESTONE_NOT_REACHED)
    
    (match user-milestone
      existing-milestone
      (asserts! (not (get claimed existing-milestone)) ERR_MILESTONE_ALREADY_CLAIMED)
      true
    )
    
    (map-set user-milestones
      { user: user, milestone-id: milestone-id }
      {
        achieved: true,
        claimed: true,
        achieved-at: stacks-block-height
      }
    )
    (ok true)
  )
)


(define-map trust-delegations
  { delegator: principal, delegate: principal }
  {
    delegation-power: uint,
    expires-at: uint,
    created-at: uint
  }
)

(define-data-var total-delegations uint u0)

(define-read-only (get-delegation (delegator principal) (delegate principal))
  (map-get? trust-delegations { delegator: delegator, delegate: delegate })
)

(define-read-only (is-delegation-active (delegator principal) (delegate principal))
  (match (get-delegation delegator delegate)
    delegation
    (ok (> (get expires-at delegation) stacks-block-height))
    (ok false)
  )
)

(define-read-only (get-effective-trust (user principal))
  (match (map-get? user-profiles { user: user })
    profile
    (let
      (
        (base-trust (get trust-score profile))
      )
      (ok base-trust)
    )
    (ok u0)
  )
)

(define-public (delegate-trust (delegate principal) (power uint) (duration uint))
  (let
    (
      (delegator tx-sender)
      (delegator-profile (unwrap! (map-get? user-profiles { user: delegator }) ERR_USER_NOT_FOUND))
      (delegate-profile (unwrap! (map-get? user-profiles { user: delegate }) ERR_USER_NOT_FOUND))
      (delegator-trust (get trust-score delegator-profile))
      (expires-at (+ stacks-block-height duration))
    )
    (asserts! (not (is-eq delegator delegate)) ERR_SELF_ENDORSEMENT)
    (asserts! (>= delegator-trust MIN_DELEGATION_TRUST) ERR_INSUFFICIENT_DELEGATION_TRUST)
    (asserts! (and (>= power u1) (<= power u20)) ERR_INVALID_WEIGHT)
    (asserts! (and (>= duration u144) (<= duration MAX_DELEGATION_DURATION)) ERR_INVALID_WEIGHT)
    (asserts! (is-none (get-delegation delegator delegate)) ERR_DELEGATION_EXISTS)
    
    (map-set trust-delegations
      { delegator: delegator, delegate: delegate }
      {
        delegation-power: power,
        expires-at: expires-at,
        created-at: stacks-block-height
      }
    )
    (var-set total-delegations (+ (var-get total-delegations) u1))
    (ok true)
  )
)

(define-public (revoke-delegation (delegate principal))
  (let
    (
      (delegator tx-sender)
      (existing-delegation (unwrap! (get-delegation delegator delegate) ERR_DELEGATION_NOT_FOUND))
    )
    (map-delete trust-delegations { delegator: delegator, delegate: delegate })
    (var-set total-delegations (- (var-get total-delegations) u1))
    (ok true)
  )
)


(define-map badge-templates
  { badge-id: uint }
  {
    name: (string-ascii 30),
    category: (string-ascii 20),
    required-endorsements: uint,
    min-trust-threshold: uint,
    rarity-level: uint
  }
)

(define-map user-badges
  { user: principal, badge-id: uint }
  {
    earned: bool,
    earned-at: uint,
    endorsement-count-when-earned: uint
  }
)

(define-map category-endorsements
  { user: principal, category: (string-ascii 20) }
  { count: uint, last-updated: uint }
)

(define-data-var next-badge-id uint u1)
(define-data-var total-badges-issued uint u0)

(define-read-only (get-badge-template (badge-id uint))
  (map-get? badge-templates { badge-id: badge-id })
)

(define-read-only (get-user-badge (user principal) (badge-id uint))
  (map-get? user-badges { user: user, badge-id: badge-id })
)

(define-read-only (get-category-endorsements (user principal) (category (string-ascii 20)))
  (default-to { count: u0, last-updated: u0 }
    (map-get? category-endorsements { user: user, category: category }))
)

(define-read-only (calculate-badge-bonus (user principal))
  (match (map-get? user-profiles { user: user })
    profile
    (let
      (
        (trust-score (get trust-score profile))
        (badge-count u0)
      )
      (ok (if (> badge-count u0) (/ badge-count u2) u0))
    )
    (ok u0)
  )
)

(define-public (create-badge-template
  (name (string-ascii 30))
  (category (string-ascii 20))
  (required-endorsements uint)
  (min-trust-threshold uint)
  (rarity-level uint))
  (let
    (
      (badge-id (var-get next-badge-id))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (and (>= rarity-level u1) (<= rarity-level u5)) ERR_INVALID_WEIGHT)
    (map-set badge-templates
      { badge-id: badge-id }
      {
        name: name,
        category: category,
        required-endorsements: required-endorsements,
        min-trust-threshold: min-trust-threshold,
        rarity-level: rarity-level
      }
    )
    (var-set next-badge-id (+ badge-id u1))
    (ok badge-id)
  )
)

(define-public (claim-badge (badge-id uint))
  (let
    (
      (user tx-sender)
      (user-profile (unwrap! (map-get? user-profiles { user: user }) ERR_USER_NOT_FOUND))
      (badge-template (unwrap! (get-badge-template badge-id) ERR_BADGE_NOT_FOUND))
      (existing-badge (map-get? user-badges { user: user, badge-id: badge-id }))
      (trust-score (get trust-score user-profile))
      (required-trust (get min-trust-threshold badge-template))
      (category (get category badge-template))
      (required-endorsements (get required-endorsements badge-template))
      (user-category-endorsements (get-category-endorsements user category))
      (category-count (get count user-category-endorsements))
    )
    (asserts! (>= trust-score required-trust) ERR_INSUFFICIENT_TRUST)
    (asserts! (>= category-count required-endorsements) ERR_INSUFFICIENT_CATEGORY_ENDORSEMENTS)
    (asserts! (is-none existing-badge) ERR_BADGE_ALREADY_EARNED)
    
    (map-set user-badges
      { user: user, badge-id: badge-id }
      {
        earned: true,
        earned-at: stacks-block-height,
        endorsement-count-when-earned: category-count
      }
    )
    (var-set total-badges-issued (+ (var-get total-badges-issued) u1))
    (ok true)
  )
)