# PhysioTrial 0.5.0

Power and sample-size for rehabilitation trial designs (`R/power.R`) — the
package could previously run and report a trial but not size or power one.

- `sampleSizeContinuous()` / `powerContinuous()` — two-arm continuous endpoint,
  cross-validated against base `stats::power.t.test`; adds unequal allocation,
  one/two-sided, dropout inflation, and cluster design effect.
- `sampleSizeANCOVA()` — the recommended pre-post analysis, baseline-adjusted
  (variance x (1 - rho^2)); `sampleSizeChangeScore()` — change-from-baseline
  (SD x sqrt(2(1 - rho))); `sampleSizeRepeatedMeasures()` — mean of repeated
  post-baseline visits under compound symmetry (Frison & Pocock).
- `sampleSizeBinary()` / `powerBinary()` — two-proportion / responder endpoint,
  matching `stats::power.prop.test`.
- `powerCurve()` for planning; `estimateBaselineCorrelation()` estimates rho for
  ANCOVA planning from pilot vectors, a data frame, or a `PhysioCohort`.
- All share cluster (ICC design effect) and dropout modifiers and return a
  printable `trial_power` object.

# PhysioTrial 0.4.0

* Adds deterministic, linked SDTM-shaped DM, AE, VS, EG, and sponsor-defined XP
  exports plus validated passthrough domains.
* Adds ADaM-shaped ADSL and generic BDS exports preserving explicit analysis-set
  membership and exclusion metadata.
* Adds structural validation and deterministic define.xml metadata stubs
  without claiming formal regulatory conformance.

# PhysioTrial 0.3.0

* Adds explicit ITT and per-protocol analysis sets with deterministic exclusion
  reasons.
* Adds source-order-preserving outcome filtering, LOCF sensitivity handling,
  and explicit multiple-imputation callbacks.
* Adds ANCOVA, delegated MMRM, and delegated MDC/MCID responder summaries.

# PhysioTrial 0.2.0

* Adds reconciled CONSORT participant-flow counts and optional DiagrammeR
  rendering.
* Adds SPIRIT schedule-of-events tables and a SPIRIT 2013 checklist helper.
* Adds typed adverse-event capture and AE/SAE summaries by arm, severity, and
  term.

# PhysioTrial 0.1.0

* Adds S4 models for trial arms, participants, randomized trials, concealed
  randomization sequences, and blinding management.
* Supports simple, permuted-block, stratified-block, and Pocock-Simon
  minimization allocation with captured seeds and deterministic fingerprints.
* Adds treatment-code management, append-only unblinding logs, and Bang
  blinding-index estimates.
