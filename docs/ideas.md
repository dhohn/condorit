# Ideas

## Getting results (users)

- **Resubmit only the failed procs of a cluster.** Collect the ProcIds
  with ExitCode ≠ 0, plus held or removed ones (`condor_history -af
  ProcId ExitCode`, `condor_q`). Generate `queue … from failed.txt` from
  the original submit file (`SubmitFile`, `Iwd`) and resubmit with a
  batch name like `scanA-retry1`.
- **Bulk fix for held jobs:** release every job held with one
  HoldReasonCode after raising the request. For example, code 34 →
  RequestMemory × 1.5 (`condor_qedit -constraint` + `condor_release`).
- **Campaign tracking by batch name.** Totals across the queue and the
  whole history (`condor_q -batch`, `condor_history -constraint
  ClusterId==N`), not only the last 24 h / 1000 jobs of "Recently
  finished". Know when a 5000-job campaign is really done.
- **Notifications without Emacs open:** offer `notification = Complete`
  / `notify_user` at submit, or `condor_wait` on the user log. Today they
  need auto-refresh, which is off by default, so out of the box they
  show only on `g`.
- **Progress and ETA per cluster:** done/total and a bar. Estimate the
  rest from median runtime × remaining ÷ running (`RemoteWallClockTime`,
  `JobCurrentStartDate`). For idle jobs, a rough start estimate from
  priority and the number of matching free slots.
- **A plain-language `a`:** sum up `-better-analyze` in one sentence,
  e.g. "your memory request rules out 80% of slots".
- **Output collection:** Dired on a cluster's Iwd or output directory,
  procs with missing or empty output, and a grep across the stderr of a
  cluster.
- **Right-sizing per cluster:** percentiles of peak memory, disk and
  wall time over finished jobs, with a suggested request for next time
  (`MemoryUsage`, `DiskUsage`).
- **"Failed" beyond exit codes:** optionally treat an exit 0 with
  missing or empty output as failed. Say in the legend what "failed"
  means.

## Submit

- **Submit-file checks before submitting:**
  - the site requirements (e.g. a site's `+ContainerOS`, `+CephFS_IO`,
    `+MaxRuntimeHours`);
  - output and log directories that don't exist yet, to avoid hold codes
    7, 12 and 22;
  - a dry-run summary: "N jobs, 4 CPU / 8G each, matches M slots now".
- **`-append` and variable overrides** in the submit menu (`-a
  'npoints=100'`) for quick scan variations.
- **`condorit-submit-mode` for `.sub` files:**
  - highlighting and completion of submit commands;
  - `condor_submit -dry-run` as a Flymake checker;
  - `C-c C-c` to submit.

## Admins

- **Pool health, read-only and opt-in** (an option or a separate view, so
  students don't pay for the queries):
  - unclaimed, Owner and draining slots (`State`, `Activity`, `Draining`,
    `DrainReason`);
  - missing machines (`condor_status -absent`, `LastHeardFrom`);
  - the negotiator cycle (`condor_status -negotiator`:
    `LastNegotiationCycleDuration0`, `…Rejections0`);
  - schedd load (`RecentDaemonCoreDutyCycle`, `TotalJobAds`).
- **Pool-wide header totals** from the schedd ads
  (`TotalRunningJobs`/`TotalIdleJobs`/`TotalHeldJobs`) in addition to the
  scoped count. Rank among submitters next to the priority.
- **Held jobs across the pool, grouped by owner and HoldReasonCode**
  (`condor_q -global -allusers -hold`), to mail users before they open
  tickets.
- **Accounting groups in the users view:** the group tree with quota,
  surplus, PriorityFactor and usage (`condor_userprio -grouprollup -l`).
  Group rows are dropped today.
- **An efficiency column preset:** `CPUsUsage/RequestCpus`,
  `RemoteSysCpu`, `NumJobStarts`, `NumShadowStarts` (jobs that keep
  restarting).
- **`-pool`** to look at another collector (a test pool, flocking
  peers).
- **Admin commands** (`condor_drain`, `condor_off`, `condor_reconfig`,
  `condor_userprio -setfactor`): at most an opt-in `condorit-admin`
  module behind a defcustom. Shown only to users in QUEUE_SUPER_USERS /
  ALLOW_ADMINISTRATOR; each needs the machine name typed in. Never in the
  default menus.

## Performance and load

- **Large queues:** `condor_q -totals` or paging for clusters above about
  10k jobs.
- **Cheaper refreshes:**
  - refresh `condor_history -allusers` only every few cycles;
  - allow a minimum auto-refresh interval;
  - allow turning off the expensive fetches.

## Onboarding

- **A "my first scan" page:**
  - the site requirements;
  - what each hold code means (reuse `condorit-jobs--hold-reasons`);
  - the TRAMP shell setup.

  A new student over TRAMP otherwise gets stuck at "Refreshing…".
