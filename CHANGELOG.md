# Changelog

## 0.2.0.0

- Pin the supported MyQue library contract to 0.2.0.0, with v1/v2 identity
  continuity and opaque consumer metadata.
- Add explicitly configured argument-vector body rendering for readable
  requirements Markdown. Renderer failures and raw Zutai fences refuse before
  remote mutation; legacy Markdown remains unchanged without a renderer.
- Materialize canonical terminal records from immutable Git snapshots through
  MyQue's store API, including terminal-only snapshots.
- Preserve retired UUID relationship/PR resolution and terminal closure reasons;
  descriptions provide offline minimal recovery data and immutable historical
  links instead of removed file links or historical-body fetching.
- Document standalone installation, renderer trust/protocol, migration, retained
  history and release validation.
