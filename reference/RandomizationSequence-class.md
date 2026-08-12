# Concealed Randomization Sequence

The full allocation table is stored internally. Public accessors mask
all rows after the reveal pointer.

## Slots

- `trial_id`:

  Trial identifier.

- `method`:

  Allocation method.

- `arms`:

  Ordered arm labels.

- `ratio`:

  Named allocation weights.

- `seed`:

  Captured random seed.

- `strata`:

  Trial stratification definition.

- `block_sizes`:

  Admissible block lengths.

- `p_bias`:

  Minimization biased-coin probability.

- `weights`:

  Named minimization factor weights.

- `imbalance`:

  Minimization discrepancy measure.

- `table`:

  Sealed allocation table.

- `audit`:

  Append-only access audit.

- `fingerprint`:

  Deterministic semantic-content hash.

- `revealed`:

  Number of revealed rows.
