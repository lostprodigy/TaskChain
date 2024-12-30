(define-map tasks
  { id: uint }
  {
    creator: principal,
    assignee: (optional principal),
    description: (buff 200),
    deadline: uint,
    reward: uint,
    completed: bool
  })

(define-map points
  { user: principal }
  { score: uint })

(define-data-var task-counter uint u0)

;; Create a task
(define-public (create-task (description (buff 200)) (deadline uint) (reward uint))
  (let ((task-id (var-get task-counter)))
    (begin
      (map-set tasks
        { id: task-id }
        {
          creator: tx-sender,
          assignee: none,
          description: description,
          deadline: deadline,
          reward: reward,
          completed: false
        })
      (var-set task-counter (+ task-id u1))
      (ok task-id)
    )
  )
)

;; Assign a task
(define-public (assign-task (task-id uint) (assignee principal))
  (match (map-get tasks { id: task-id })
    task
    (if (is-eq (get creator task) tx-sender)
        (begin
          (map-set tasks { id: task-id } (merge task { assignee: (some assignee) }))
          (ok "Task assigned successfully")
        )
        (err u403))
    (err u404)
  )
)

;; Mark task as complete
(define-public (complete-task (task-id uint))
  (match (map-get tasks { id: task-id })
    task
    (if (and (is-some (get assignee task)) (is-eq (unwrap! (get assignee task) u404) tx-sender))
        (if (not (get completed task))
            (begin
              (map-set tasks { id: task-id } (merge task { completed: true }))
              (let ((current-score (default-to u0 (get score (map-get points { user: tx-sender })))))
                (map-set points { user: tx-sender } { score: (+ current-score (get reward task)) })
              )
              (ok "Task completed")
            )
            (err u400))
        (err u403))
    (err u404)
  )
)

;; View all tasks
(define-read-only (get-tasks)
  (map tasks (fn (key { id: uint } value { creator: principal, assignee: (optional principal), description: (buff 200), deadline: uint, reward: uint, completed: bool }) value))
)

;; View user points
(define-read-only (get-user-points (user principal))
  (default-to u0 (get score (map-get points { user: user })))
)
