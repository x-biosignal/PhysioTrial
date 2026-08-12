# Changelog

## PhysioTrial 0.4.0

- Adds deterministic, linked SDTM-shaped DM, AE, VS, EG, and
  sponsor-defined XP exports plus validated passthrough domains.
- Adds ADaM-shaped ADSL and generic BDS exports preserving explicit
  analysis-set membership and exclusion metadata.
- Adds structural validation and deterministic define.xml metadata stubs
  without claiming formal regulatory conformance.

## PhysioTrial 0.3.0

- Adds explicit ITT and per-protocol analysis sets with deterministic
  exclusion reasons.
- Adds source-order-preserving outcome filtering, LOCF sensitivity
  handling, and explicit multiple-imputation callbacks.
- Adds ANCOVA, delegated MMRM, and delegated MDC/MCID responder
  summaries.

## PhysioTrial 0.2.0

- Adds reconciled CONSORT participant-flow counts and optional
  DiagrammeR rendering.
- Adds SPIRIT schedule-of-events tables and a SPIRIT 2013 checklist
  helper.
- Adds typed adverse-event capture and AE/SAE summaries by arm,
  severity, and term.

## PhysioTrial 0.1.0

- Adds S4 models for trial arms, participants, randomized trials,
  concealed randomization sequences, and blinding management.
- Supports simple, permuted-block, stratified-block, and Pocock-Simon
  minimization allocation with captured seeds and deterministic
  fingerprints.
- Adds treatment-code management, append-only unblinding logs, and Bang
  blinding-index estimates.
