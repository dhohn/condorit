# To do

- **New HTCondor versions in CI.** `make record TAG=…` records every view
  from a pool in `htcondor/mini:TAG`, and the version tests replay all
  recorded versions. Once condorit is on a forge, a weekly job should
  record `latest`. If the new recording fails the version tests, or its
  attributes differ from the newest recorded version, the job should fail
  and upload it, to be committed as the next version.
- **Actions against the live pool.** The recordings cover the views only.
  Hold, release, edit and remove, and their error messages (e.g.
  `condor_rm` of a job that is gone), are still tested only against the
  fake condor.
