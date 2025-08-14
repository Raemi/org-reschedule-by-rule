# org-reschedule-by-rule

## Overview

`org-reschedule-by-rule` is an Emacs package for **automatic, rule-based
rescheduling of Org-mode tasks**. It lets you define **cron expressions** or
**time intervals** (or both) that determine the next scheduled date when a task
is marked `DONE`.

Unlike Org’s built-in repeaters, these rules:

* **Ignore** the current scheduled date when rescheduling.
* Use a stable **anchor date** (for intervals), so you can freely move tasks
  around day-to-day without breaking their long-term pattern.
* Always bring the task back into alignment with your intended schedule after
  completion.

### Why?

In vanilla Org, if you have:

```org
SCHEDULED: <2025-09-01 Mon ++1m>
```

and you manually move it from the 1st to the 2nd this month, marking it `DONE`
will push it to the 2nd next month — not the 1st. This package solves that by
enforcing your intended cadence no matter where you drag the task in the short
term.

This is especially handy for:

* Recurring meetings or deadlines that must always fall on a certain weekday or
  calendar rule.
* Habit-style tasks where you want to adjust day-to-day but keep the big picture
  fixed.
* "Inbox zero" workflows where you freely move tasks to the next day, without
  breaking recurring patterns.

---

## Requirements

You'll need:

- **Python 3**
- The [`croniter`](https://github.com/pallets-eco/croniter) Python package
- The `python-dateutil` Python package

### Emacs configuration

After cloning the repository, configure it in your Emacs init. For example:

```elisp
(use-package org-reschedule-by-rule
  :load-path "/path/to/org-reschedule-by-rule"
  :after org)
```

---

## Usage

### 1. Cron-based rescheduling
Set the `RESCHEDULE_CRON` property to a cron expression.

- **3-field** (`DOM MON DOW`) → date accuracy (no time)
- **5-field** (`MIN HOUR DOM MON DOW`) → date + time accuracy

See [`croniter` docs](https://github.com/pallets-eco/croniter) for full syntax.

Example: always reschedule to Monday

```org
* TODO Weekly review
SCHEDULED: <2025-08-12 Tue>
:PROPERTIES:
:RESCHEDULE_CRON: * * Mon
:END:
```

If you move it to a Tuesday, it will jump to the *next Monday* after you mark it
`DONE`.


---

### 2. Interval-based rescheduling

Set the `RESCHEDULE_INTERVAL` property (e.g. `1h`, `2d`, `3w`, `4m`, `5y`).

- First reschedule stores an anchor date in `RESCHEDULE_ANCHOR`
- Future reschedules count the interval from that anchor — not from the current
  `SCHEDULED` date

Example: anchored to Wednesday every week

```org
* TODO Team sync
SCHEDULED: <2025-08-14 Thu>
:PROPERTIES:
:RESCHEDULE_ANCHOR: 2025-08-13 Wed
:RESCHEDULE_INTERVAL: 1w
:END:
```

Even if moved mid-week, `DONE` will push it to the *next Wednesday*.

---

### 3. Combining interval + cron (advanced usage)

Both rules can be set. The interval advances from the anchor until the cron
constraint is satisfied.

Example: every 2 days, but only Mon/Tue/Wed

```org
* TODO Report update
SCHEDULED: <2025-08-03 Sun>
:PROPERTIES:
:RESCHEDULE_INTERVAL: 2d
:RESCHEDULE_CRON: * * Mon,Tue,Wed
:END:
```

---

### 4. More examples

- **Last Monday of the month**
    ```org
    :RESCHEDULE_CRON: * * L1
    ```

    `L` means "last day of month", the `1` refers to the first day of the week

- **9 AM every Monday**
    ```org
    :RESCHEDULE_CRON: 0 9 * * Mon
    ```

- **Third Thursday of the month**
    ```org
    :RESCHEDULE_CRON: * * Thu#3
    ```

- **Next Monday or Friday**
    ```org
    :RESCHEDULE_CRON: * * Mon,Fri
    ```

- **Every 2 days from a specific anchor**
    ```org
    :RESCHEDULE_INTERVAL: 2d
    :RESCHEDULE_ANCHOR: 2025-08-03 Sun
    ```

- **Every first Monday of the quarter**
    ```org
    :RESCHEDULE_CRON: * Jan,Apr,Jul,Oct Mon#1
    ```

---



## License

This software is released under the MIT license.
