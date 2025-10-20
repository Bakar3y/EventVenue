;; EventVenue: Decentralized Event Management Platform
;; Version: 1.0.0

(define-constant ERR-UNAUTHORIZED-ACCESS (err u1))
(define-constant ERR-EVENT-NOT-EXISTS (err u2))
(define-constant ERR-ALREADY-PUBLISHED (err u3))
(define-constant ERR-INVALID-STATE (err u4))
(define-constant ERR-INVALID-CAPACITY (err u5))
(define-constant ERR-INVALID-EVENT-TYPE (err u6))
(define-constant ERR-INVALID-VENUE-CLASS (err u7))
(define-constant ERR-INVALID-EVENT-NAME (err u8))
(define-constant ERR-INVALID-VENUE-INFO (err u9))

(define-constant MIN-CAPACITY u10)

(define-data-var next-event-id uint u1)

(define-map event-listings
    uint
    {
        organizer: principal,
        event-name: (string-utf8 50),
        venue-info: (string-utf8 200),
        event-type: (string-utf8 15),
        venue-class: (string-utf8 10),
        publication-status: (string-utf8 15),
        capacity-count: uint
    }
)

(define-private (validate-event-type (event-type (string-utf8 15)))
    (or 
        (is-eq event-type u"Conference")
        (is-eq event-type u"Concert")
        (is-eq event-type u"Workshop")
        (is-eq event-type u"Networking")
        (is-eq event-type u"Festival")
        (is-eq event-type u"Seminar")
    )
)

(define-private (validate-venue-class (venue-class (string-utf8 10)))
    (or 
        (is-eq venue-class u"Intimate")
        (is-eq venue-class u"Small")
        (is-eq venue-class u"Medium")
        (is-eq venue-class u"Large")
        (is-eq venue-class u"Massive")
    )
)

(define-private (validate-text-content (text (string-utf8 200)) (min-length uint) (max-length uint))
    (let 
        (
            (text-length (len text))
        )
        (and 
            (>= text-length min-length)
            (<= text-length max-length)
        )
    )
)

(define-public (publish-event 
    (event-name (string-utf8 50))
    (venue-info (string-utf8 200))
    (event-type (string-utf8 15))
    (venue-class (string-utf8 10))
    (capacity-count uint)
)
    (let
        (
            (event-id (var-get next-event-id))
        )
        (asserts! (validate-text-content event-name u3 u50) ERR-INVALID-EVENT-NAME)
        (asserts! (validate-text-content venue-info u10 u200) ERR-INVALID-VENUE-INFO)
        (asserts! (>= capacity-count MIN-CAPACITY) ERR-INVALID-CAPACITY)
        (asserts! (validate-event-type event-type) ERR-INVALID-EVENT-TYPE)
        (asserts! (validate-venue-class venue-class) ERR-INVALID-VENUE-CLASS)
        
        (map-set event-listings event-id {
            organizer: tx-sender,
            event-name: event-name,
            venue-info: venue-info,
            event-type: event-type,
            venue-class: venue-class,
            publication-status: u"published",
            capacity-count: capacity-count
        })
        (var-set next-event-id (+ event-id u1))
        (ok event-id)
    )
)

(define-public (unpublish-event (event-id uint))
    (let
        (
            (event (unwrap! (map-get? event-listings event-id) ERR-EVENT-NOT-EXISTS))
        )
        (asserts! (is-eq tx-sender (get organizer event)) ERR-UNAUTHORIZED-ACCESS)
        (asserts! (is-eq (get publication-status event) u"published") ERR-INVALID-STATE)
        (ok (map-set event-listings event-id (merge event { publication-status: u"cancelled" })))
    )
)

(define-read-only (get-event (event-id uint))
    (ok (map-get? event-listings event-id))
)

(define-read-only (get-organizer (event-id uint))
    (ok (get organizer (unwrap! (map-get? event-listings event-id) ERR-EVENT-NOT-EXISTS)))
)