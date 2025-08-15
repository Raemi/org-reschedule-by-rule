;;; org-reschedule-by-rule.el --- Automatic, rule-based rescheduling of Org-mode tasks using cron or interval rules -*- lexical-binding: t; -*-
;;
;; org-reschedule-by-rule provides automatic, rule-based rescheduling of
;; Org-mode tasks using per-heading cron expressions or time intervals. It
;; anchors intervals to a stable date, ensuring recurring tasks always align
;; with your intended schedule, regardless of manual adjustments.
;;
;; Leveraging Python's croniter library for cron constraints and using a
;; dedicated RESCHEDULE_ANCHOR property for intervals, this package decouples
;; the next scheduled date from the current SCHEDULED timestamp, offering more
;; predictable and reliable rescheduling than Org's built-in repeaters.
;;
(require 'org)
(require 'subr-x)
(require 'cl-lib)


(defgroup org-reschedule-by-rule nil
  "Reschedule Org tasks according to per-heading cron or interval rules."
  :group 'org)

(defcustom org-reschedule-by-rule-anchor-prop "RESCHEDULE_ANCHOR"
  "Property holding the last rescheduled timestamp. This serves as an
anchor for rescheduling, in particular to make 'RESCHEDULE_INTERVAL'
independent of the current 'SCHEDULED' timestamp. The
'RESCHEDULE_ANCHOR' is updated every time a rescheduling happened."
  :type 'string
  :group 'org-reschedule-by-rule)

(defcustom org-reschedule-by-rule-interval-prop "RESCHEDULE_INTERVAL"
  "Property holding an interval for rescheduling. The rescheduling
happens relative to the 'RESCHEDULE_ANCHOR' and is therefore independent
of the current 'SCHEDULED' timestamp. Format: '[0-9]+[h|d|w|m|y]'."
  :type 'string
  :group 'org-reschedule-by-rule)

(defcustom org-reschedule-by-rule-cron-prop "RESCHEDULE_CRON"
  "Property holding a day-only cron expression (DOM MON DOW), or a
full (5-field) (min hour DOM MON DOW). The CRON expression restricts the
possible dates to reschedule the task."
  :type 'string
  :group 'org-reschedule-by-rule)

(defvar org-reschedule-by-rule-python
  (or (executable-find "python3") (executable-find "python"))
  "Path to Python interpreter for croniter (python3 preferred).")


(defun org-reschedule-by-rule--cron-rule-arity (rule)
  "Return number of fields in RULE (3 or 5), or nil if invalid."
  (let* ((parts (split-string (string-trim rule) "[ \t]+" t))
         (n (length parts)))
    (cond ((= n 3) 3)
          ((= n 5) 5)
          (t nil))))

(defun org-reschedule-by-rule--normalize-cron-rule (rule)
  "Normalize RULE into a 5-field cron for croniter.
Accepts 3 fields (DOM MON DOW) or 5 fields (MIN HOUR DOM MON DOW)."
  (let* ((parts (split-string (string-trim rule) "[ \t]+" t)))
    (pcase (length parts)
      (3 (concat "0 0 " (string-trim rule)))
      (5 (string-trim rule))
      (_ (progn
           (message "org-reschedule-by-rule: invalid cron rule: %S" rule)
           nil)))))

(defun org-reschedule-by-rule--next-date-from-cron (base-time now-time &optional expr interval)
  "Return an Emacs time for the next run using a universal Python scheduler.
Handles special 'Nth weekday' logic, standard cron, and simple intervals."
  (let* ((py org-reschedule-by-rule-python)
         (code (concat
                "import sys\n"
                "import re\n"
                "from datetime import datetime, date, timedelta\n"
                "from dateutil.relativedelta import relativedelta\n"
                "import croniter\n"
                "\n"
                "def find_nth_weekday(year, month, target_weekday, n):\n"
                "    try:\n"
                "        first_day_of_month_weekday = date(year, month, 1).weekday()\n"
                "        day_of_first_occurrence = (target_weekday - first_day_of_month_weekday + 7) % 7 + 1\n"
                "        day_of_nth_occurrence = day_of_first_occurrence + (n - 1) * 7\n"
                "        return date(year, month, day_of_nth_occurrence)\n"
                "    except ValueError:\n"
                "        return None\n"
                "\n"
                "MAX_ATTEMPTS = 1000\n"
                "base = datetime.strptime(sys.argv[1], '%Y-%m-%d %H:%M:%S')\n"
                "now_dt = datetime.strptime(sys.argv[2], '%Y-%m-%d %H:%M:%S')\n"
                "cron_expr = sys.argv[3] if len(sys.argv) > 3 else ''\n"
                "interval_str = sys.argv[4] if len(sys.argv) > 4 else ''\n"
                "\n"
                "next_time = None\n"
                "is_special_case = False\n"
                "\n"
                "# --- SPECIAL LOGIC for 'First Monday of the 3rd Month' ---\n"
                "if interval_str and (interval_str.endswith('m') or interval_str.endswith('y')) and re.search(r'[a-zA-Z]{3}#\\d', cron_expr):\n"
                "    is_special_case = True\n"
                "    match = re.search(r'([a-zA-Z]{3})#([1-5])', cron_expr)\n"
                "    day_str, nth_str = match.groups()\n"
                "    nth = int(nth_str)\n"
                "    weekdays = {'sun': 6, 'mon': 0, 'tue': 1, 'wed': 2, 'thu': 3, 'fri': 4, 'sat': 5}\n"
                "    target_weekday = weekdays.get(day_str.lower())\n"
                "\n"
                "    num_match = re.search(r'(\\d+)', interval_str)\n"
                "    num = int(num_match.group(1)) if num_match else 1\n"
                "    unit = interval_str[-1]\n"
                "    delta = relativedelta(months=num) if unit == 'm' else relativedelta(years=num)\n"
                "\n"
                "    search_dt = base\n"
                "    count = 0\n"
                "    while count < MAX_ATTEMPTS:\n"
                "        search_dt += delta\n"
                "        next_date_part = find_nth_weekday(search_dt.year, search_dt.month, target_weekday, nth)\n"
                "        if next_date_part:\n"
                "            candidate = datetime.combine(next_date_part, base.time())\n"
                "            if candidate > base:\n"
                "                next_time = candidate\n"
                "                break\n"
                "        count += 1\n"
                "\n"
                "# --- GENERAL LOGIC for all other cases ---\n"
                "if not is_special_case:\n"
                "    search_start = base\n"
                "\n"
                "    if interval_str:\n"
                "        match = re.search(r'(\\d+)([hdwmy])', interval_str)\n"
                "        if match:\n"
                "            num, unit = int(match.group(1)), match.group(2)\n"
                "            delta = relativedelta(hours=num) if unit == 'h' else \\\n"
                "                    relativedelta(days=num) if unit == 'd' else \\\n"
                "                    relativedelta(weeks=num) if unit == 'w' else \\\n"
                "                    relativedelta(months=num) if unit == 'm' else \\\n"
                "                    relativedelta(years=num)\n"
                "            if delta: search_start += delta\n"
                "\n"
                "    if cron_expr:\n"
                "        final_cron_expr = cron_expr\n"
                "        parts = cron_expr.split()\n"
                "        # **THE FIX**: If minute is '*' and there's no interval, use the original minute.\n"
                "        if len(parts) == 5 and parts[0] == '*' and not interval_str:\n"
                "            parts[0] = str(base.minute)\n"
                "            final_cron_expr = ' '.join(parts)\n"
                "\n"
                "        # Start the search from the original scheduled time.\n"
                "        itr = croniter.croniter(final_cron_expr, base)\n"
                "        candidate = itr.get_next(datetime)\n"
                "\n"
                "        # Ensure the next date is always in the future.\n"
                "        while candidate <= now_dt:\n"
                "            candidate = itr.get_next(datetime)\n"
                "        next_time = candidate\n"
                "    else:\n"
                "        # No cron, just use the date with the interval applied.\n"
                "        next_time = search_start\n"
                "\n"
                "if next_time:\n"
                "    print(next_time.strftime('%Y-%m-%d %H:%M:%S'))\n"
                "else:\n"
                "    sys.stderr.write(\"Could not find a valid reschedule date.\")\n"
                "    sys.exit(1)\n"
                ""))
         (buf (generate-new-buffer " *croniter-universal*"))
         (args (append
                (list "-c" code)
                (list (format-time-string "%Y-%m-%d %H:%M:%S" base-time))
                (list (format-time-string "%Y-%m-%d %H:%M:%S" now-time))
                (list (or expr ""))
                (when interval (list interval))))
         exit out)
    (unwind-protect
        (progn
          (unless org-reschedule-by-rule-python (user-error "python3/python not found on PATH"))
          (setq exit (apply #'call-process py nil buf nil args))
          (setq out (with-current-buffer buf (buffer-string)))
          (if (and (integerp exit) (= exit 0))
              (let* ((ts (string-trim out))
                     (dt (parse-time-string ts)))
                (apply #'encode-time dt))
            (warn "org-reschedule-by-rule: scheduler failed (%s): %s"
                  exit (string-trim (or out "")))
            nil))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(defun org-reschedule-by-rule--reschedule-use-time-p (interval anchor-str cron-arity scheduled-str)
  "Return a format string for the rescheduled timestamp.
Choose format based on the first existing timestamp or rule:
1. If INTERVAL is in hours, include time.
2. Else if ANCHOR exists, include time if it has HH:MM.
3. Else if CRON is 5-field, include time.
4. Else if SCHEDULED exists, include time if it has HH:MM.
Otherwise, date only."
  (let ((fmt-time  "%Y-%m-%d %a %H:%M")
        (fmt-date  "%Y-%m-%d %a")
        (time-regexp "[0-9]\\{2\\}:[0-9]\\{2\\}"))
    (cond
     ((and interval (string-match-p "[0-9]+h\\'" interval))
      fmt-time)
     (anchor-str
      (if (string-match-p time-regexp anchor-str)
          fmt-time
        fmt-date))
     ((eq cron-arity 5)
      fmt-time)
     (scheduled-str
      (if (string-match-p time-regexp scheduled-str)
          fmt-time
        fmt-date))
     (t fmt-date))))



;;;###autoload
(defun org-reschedule-by-rule-on-done ()
  "When a heading is DONE, reschedule it per RESCHEDULE_CRON/INTERVAL."
  (when (org-entry-is-done-p)
    (save-excursion
      (org-back-to-heading t)
      (let* ((interval-str   (org-entry-get (point) org-reschedule-by-rule-interval-prop nil))
             (cron-str       (org-entry-get (point) org-reschedule-by-rule-cron-prop nil))
             (has-interval   (and interval-str (> (length (string-trim interval-str)) 0)))
             (has-cron       (and cron-str     (> (length (string-trim cron-str))     0)))
             (sched-time     (org-get-scheduled-time (point)))
             (anchor-str     (org-entry-get (point) org-reschedule-by-rule-anchor-prop nil))
             (anchor-time    (when (and anchor-str (> (length (string-trim anchor-str)) 0))
                               (org-time-string-to-time anchor-str)))
             (base           (or anchor-time sched-time (current-time)))
             (trim-interval  (when has-interval (string-trim interval-str)))
             (trim-cron      (when has-cron     (string-trim cron-str)))
             (cron-arity          (when has-cron     (org-reschedule-by-rule--cron-rule-arity trim-cron)))
             (norm-cron      (when cron-arity        (org-reschedule-by-rule--normalize-cron-rule trim-cron)))
             (cron-param     (when cron-arity        norm-cron))
             (scheduled-str  (org-entry-get (point) "SCHEDULED"))
             (fmt            (org-reschedule-by-rule--reschedule-use-time-p
                              trim-interval anchor-str cron-arity scheduled-str)))
        (if (not (or has-cron has-interval))
            (message "[resched] no RESCHEDULE_CRON or RESCHEDULE_INTERVAL, skipping")
          (progn
            (when (and has-interval
                       (not (string-match-p "\\`[0-9]+[hdwmy]\\'" trim-interval)))
              (message "[resched] invalid RESCHEDULE_INTERVAL %S, skipping"
                       trim-interval)
              (cl-return-from org-reschedule-by-rule-on-done))
            (when (and has-cron (null cron-arity))
              (message "[resched] invalid cron rule, skipping"))
            (let* ((now (current-time))
                   (next (org-reschedule-by-rule--next-date-from-cron base now cron-param trim-interval)))
              (when next
                (org-schedule nil (format-time-string fmt next))
                (org-entry-put (point) org-reschedule-by-rule-anchor-prop
                               (format-time-string fmt next))
                (org-todo 'todo)
                (message "[resched] rescheduled to %s%s"
                         (format-time-string fmt next)
                         (if has-cron ", reset state" " by interval, reset state"))))))))))

(add-hook 'org-after-todo-state-change-hook #'org-reschedule-by-rule-on-done)

(provide 'org-reschedule-by-rule)

;;; org-reschedule-by-rule.el ends here
