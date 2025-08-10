;;; org-reschedule-by-rule-tests.el --- Unit tests for org-reschedule-by-rule  -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'org-reschedule-by-rule)

;;; --------------------------------------------------------------------------
;;; Tests for org-reschedule-by-rule--cron-rule-arity

(ert-deftest cron-arity-returns-3-for-day-only ()
  (should (= (org-reschedule-by-rule--cron-rule-arity "1 * Mon,Fri-Sun") 3))
  (should (= (org-reschedule-by-rule--cron-rule-arity "*\t2\t3") 3)))

(ert-deftest cron-arity-returns-5-for-full ()
  (should (= (org-reschedule-by-rule--cron-rule-arity "0 12 * * Mon") 5)))

(ert-deftest cron-arity-returns-nil-for-invalid ()
  (should-not (org-reschedule-by-rule--cron-rule-arity "1 2"))
  (should-not (org-reschedule-by-rule--cron-rule-arity "")))

;;; --------------------------------------------------------------------------
;;; Tests for org-reschedule-by-rule--normalize-cron-rule

(ert-deftest normalize-cron-3-fields ()
  (should (string= (org-reschedule-by-rule--normalize-cron-rule "1 2 3")
                   "0 0 1 2 3")))

(ert-deftest normalize-cron-5-fields ()
  (should (string= (org-reschedule-by-rule--normalize-cron-rule "0 12 1 2 3")
                   "0 12 1 2 3")))

(ert-deftest normalize-cron-invalid-returns-nil ()
  (should-not (org-reschedule-by-rule--normalize-cron-rule "1 2")))

;;; --------------------------------------------------------------------------
;;; Tests for org-reschedule-by-rule--reschedule-use-time-p

(ert-deftest test-org-reschedule-by-rule--reschedule-use-time-p ()
  ;; Test when INTERVAL is defined
  (should (equal (org-reschedule-by-rule--reschedule-use-time-p "2h" nil nil nil) "<%Y-%m-%d %a %H:%M>"))
  (should (equal (org-reschedule-by-rule--reschedule-use-time-p "2d" nil nil nil) "<%Y-%m-%d %a>"))

  ;; Test when ANCHOR exists
  (should (equal (org-reschedule-by-rule--reschedule-use-time-p nil "2023-10-14 15:30" nil nil) "<%Y-%m-%d %a %H:%M>"))
  (should (equal (org-reschedule-by-rule--reschedule-use-time-p nil "2023-10-14" nil nil) "<%Y-%m-%d %a>"))

  ;; Test when CRON exists
  (should (equal (org-reschedule-by-rule--reschedule-use-time-p nil nil 5 nil) "<%Y-%m-%d %a %H:%M>"))
  (should (equal (org-reschedule-by-rule--reschedule-use-time-p nil nil 3 nil) "<%Y-%m-%d %a>"))

  ;; Test when SCHEDULED exists
  (should (equal (org-reschedule-by-rule--reschedule-use-time-p nil nil nil "2023-10-14 09:00") "<%Y-%m-%d %a %H:%M>"))
  (should (equal (org-reschedule-by-rule--reschedule-use-time-p nil nil nil "2023-10-14") "<%Y-%m-%d %a>"))

  ;; Test when no criteria match
  (should (equal (org-reschedule-by-rule--reschedule-use-time-p nil nil nil nil) "<%Y-%m-%d %a>"))
  )


;;; --------------------------------------------------------------------------
;;; Tests for org-reschedule-by-rule-on-done
;; We'll create a temporary Org buffer, simulate a DONE heading,
;; stub out org-reschedule-by-rule--next-date-from-cron so it's deterministic.

(ert-deftest resched-on-done-updates-scheduled-and-anchor ()
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Test task\n")
    (org-schedule nil "2025-08-08 Fri 12:00")
    (org-entry-put (point-min) org-reschedule-by-rule-interval-prop "1d")
    ;; Stub next-date so it's deterministic
    (cl-letf (((symbol-function 'org-reschedule-by-rule--next-date-from-cron)
               (lambda (&rest _) (encode-time 0 0 12 10 8 2025))))
      ;; This triggers org-after-todo-state-change-hook, binding org-state
      (goto-char (point-min))
      (org-todo 'done))
    ;; Check results
    (goto-char (point-min))
    (let ((sched  (org-entry-get (point) "SCHEDULED"))
          (anchor (org-entry-get (point) org-reschedule-by-rule-anchor-prop)))
      (should (string-match "2025-08-10" sched))
      (should (string= sched anchor)))
    (should (string= (org-get-todo-state) "TODO"))))



;;; --------------------------------------------------------------------------
;;; Tests for org-reschedule-by-rule--next-date-from-cron
;; We'll stub out call-process so we don't need Python in tests.

(ert-deftest next-date-from-cron-fails-gracefully ()
  (cl-letf (((symbol-function 'call-process)
             (lambda (_prog _in out _display &rest _args)
               (with-current-buffer out
                 (insert "Something went wrong"))
               1))) ; nonzero exit
    (should-not (org-reschedule-by-rule--next-date-from-cron
                 (current-time)
                 (current-time)
                 "* * * * *"
                 nil))))


;; Small helper to make times concise
(defun test-time (Y M D h m s)
  (encode-time s m h D M Y))


;;; ----------------------------------------------------------------------
;;; CRON-ONLY

(ert-deftest next-date-cron-only-simple ()
  "From base==now morning to next 12:00 today."
  (let* ((base (test-time 2025 8 8 10 0 0))
         (now  base)
         (expr "0 12 * * *")
         (res  (org-reschedule-by-rule--next-date-from-cron base now expr nil)))
    (should (equal (format-time-string "%Y-%m-%d %H:%M" res)
                   "2025-08-08 12:00"))
    ))

(ert-deftest next-date-cron-day-of-week ()
  "From base==now, a Fri to next Mon."
  (let* ((base (test-time 2025 8 8 10 0 0))
         (now  base)
         (expr "0 0 * * Mon-Fri")
         (res  (org-reschedule-by-rule--next-date-from-cron base now expr nil)))
    (should (equal (format-time-string "%Y-%m-%d %H:%M" res)
                   "2025-08-11 00:00"))))

(ert-deftest next-date-cron-only-skip-equal-now ()
  "If the next cron equals NOW, code must advance to strictly after NOW."
  (let* ((base (test-time 2025 8 8 0 0 0))
         (now  (test-time 2025 8 8 12 0 0))
         (expr "0 12 * * *")
         (res  (org-reschedule-by-rule--next-date-from-cron base now expr nil)))
    ;; Should roll to tomorrow 12:00
    (should (equal (format-time-string "%Y-%m-%d %H:%M" res)
                   "2025-08-09 12:00"))
    ))

(ert-deftest next-date-cron-example-cases ()
  "A bunch of cron expressions that are supported"
  (let* ((base (test-time 2025 8 8 0 0 0))
         (now  base))

    ;; Last Monday:
    (let ((res (org-reschedule-by-rule--next-date-from-cron base now "0 0 * * L1" nil)))
      (should (equal (format-time-string "%Y-%m-%d %H:%M" res)
                     "2025-08-25 00:00")))
    ;; At 9:00 on Mondays:
    (let ((res (org-reschedule-by-rule--next-date-from-cron base now "0 9 * * Mon" nil)))
      (should (equal (format-time-string "%Y-%m-%d %H:%M" res)
                     "2025-08-11 09:00")))
    ;; Third Thursday of the month:
    (let ((res (org-reschedule-by-rule--next-date-from-cron base now "0 0 * * Thu#3" nil)))
      (should (equal (format-time-string "%Y-%m-%d %H:%M" res)
                     "2025-08-21 00:00")))
    ;; Selected weekdays:
    (let ((res (org-reschedule-by-rule--next-date-from-cron base now "0 0 * * Tue,Wed" nil)))
      (should (equal (format-time-string "%Y-%m-%d %H:%M" res)
                     "2025-08-12 00:00")))))

;;; ----------------------------------------------------------------------
;;; INTERVAL-ONLY

(ert-deftest next-date-interval-days-roll-forward ()
  "Interval 2d repeatedly added until > now."
  (let* ((base (test-time 2025 8 1 9 0 0))
         (now  (test-time 2025 8 8 10 0 0))
         (res  (org-reschedule-by-rule--next-date-from-cron base now nil "2d")))
    ;; base+2d=Aug3 09:00, +2d=Aug5 09:00, +2d=Aug7 09:00, +2d=Aug9 09:00 (> now)
    (should (equal (format-time-string "%Y-%m-%d %H:%M" res)
                   "2025-08-09 09:00"))
    ))

(ert-deftest next-date-interval-hours ()
  "Interval in hours should produce the right next candidate."
  (let* ((base (test-time 2025 8 8 9 0 0))
         (now  (test-time 2025 8 8 10 0 0))
         (res  (org-reschedule-by-rule--next-date-from-cron base now nil "3h")))
    (should (equal (format-time-string "%Y-%m-%d %H:%M" res)
                   "2025-08-08 12:00"))
    ))

(ert-deftest next-date-interval-weeks ()
  "Verify the script's 'w' = weeks handling."
  (let* ((base (test-time 2025 8 8 10 0 0))
         (now  base)
         (res  (org-reschedule-by-rule--next-date-from-cron base now nil "1w")))
    (should (equal (format-time-string "%Y-%m-%d %H:%M" res)
                   "2025-08-15 10:00"))
    ))

(ert-deftest next-date-interval-months ()
  "Verify the script's 'm' = months handling."
  (let* ((base (test-time 2025 8 8 10 0 0))
         (now  base)
         (res  (org-reschedule-by-rule--next-date-from-cron base now nil "1m")))
    (should (equal (format-time-string "%Y-%m-%d %H:%M" res)
                   "2025-09-08 10:00"))
    ))

(ert-deftest next-date-interval-years ()
  "Verify the script's 'y' = years handling."
  (let* ((base (test-time 2025 8 8 10 0 0))
         (now  base)
         (res  (org-reschedule-by-rule--next-date-from-cron base now nil "1y")))
    (should (equal (format-time-string "%Y-%m-%d %H:%M" res)
                   "2026-08-08 10:00"))
    ))


;;; ----------------------------------------------------------------------
;;; INTERVAL + CRON (both must be satisfied)

(ert-deftest next-date-interval-with-cron-must-satisfy-both ()
  "Interval 2d with cron 0 0 * * Mon-Fri lands on Tue."
  (let* ((base (test-time 2025 8 8 0 0 0))
         (now  base)
         ;; candidate = base + 1d = Aug 9 09:00, matches cron and > now
         (res  (org-reschedule-by-rule--next-date-from-cron base now "0 0 * * Mon-Fri" "2d")))
    (should (equal (format-time-string "%Y-%m-%d %H:%M" res)
                   "2025-08-12 00:00"))
    ))

;;; ----------------------------------------------------------------------
;;; ERROR / EDGE cases

(ert-deftest next-date-no-cron-no-interval-returns-nil ()
  "Without cron expr OR interval, Python exits nonzero; elisp should return nil."
  (let* ((base (test-time 2025 8 8 10 0 0))
         (now  (test-time 2025 8 8 10 0 1))
         ;; Silence wrapper warning for this negative case
         (warning-minimum-level :emergency)
         (res (org-reschedule-by-rule--next-date-from-cron base now nil nil)))
    (should-not res)))

(ert-deftest next-date-interval-leap-year-feb ()
  "Leap years shouldn't affect the date with '1m' interval."
  (let* ((base (test-time 2024 2 1 0 0 0))
         (now  base)
         (res  (org-reschedule-by-rule--next-date-from-cron base now nil "1m")))
    (should (equal (format-time-string "%Y-%m-%d %H:%M" res)
                   "2024-03-01 00:00"))
    ))


(ert-deftest next-date-interval-leap-years ()
  "Leap years shouldn't affect the date with '1y' interval."
  (let* ((base (test-time 2024 1 1 0 0 0))
         (now  base)
         (res  (org-reschedule-by-rule--next-date-from-cron base now nil "1y")))
    (should (equal (format-time-string "%Y-%m-%d %H:%M" res)
                   "2025-01-01 00:00"))
    ))


;;; --------------------------------------------------------------------------

(provide 'org-reschedule-by-rule-tests)
;;; org-reschedule-by-rule-tests.el ends here
