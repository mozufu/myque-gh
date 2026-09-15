# myque-gh 0.2

`myque-gh` projects canonical MyQue records into GitHub issues and native
milestones and links pull requests by canonical UUID. GitHub is a projection,
not a second work-state store. Licensed BSD-3-Clause.

## Supported installation

This release requires **MyQue 0.2.0.0 exactly** and supports both `work-item/v1`
and `work-item/v2`. The Cabal dependency and Nix derivation declare that contract.
The release maintainer must update `cabal.project`, the `myque` flake input and
`flake.lock` to the verified published MyQue revision before publishing this
release. A previous 0.1 revision cannot satisfy the new library dependency.

After the release is published, install from the public source repository:

```sh
nix profile install github:mozufu/myque-gh
# Or, from a checkout of the published revision:
cabal install exe:myque-gh
```

The distribution contains the library, CLI, hosted smoke executable and behavior
tests. It has no Slime OS dependency. The renderer is a separately installed,
consumer-selected program; it is intentionally not bundled as an implicit hook.
Install a compatible devloop release for structured requirements projection.

## Projection

```sh
myque-gh plan --store . --repo owner/repository --issue-author projection-bot \
  --ref HEAD --source-repo owner/repository --source-branch main

myque-gh apply --store . --repo owner/repository --issue-author projection-bot \
  --ref refs/heads/main --source-repo owner/repository --source-branch main \
  --body-renderer devloop --body-renderer-arg render
```

Both `plan` and `apply` accept:

- `--body-renderer EXECUTABLE`: explicitly trusted executable, not a shell
  command. Use an absolute installation path for stronger executable pinning.
- `--body-renderer-arg ARG`: repeatable literal argument, preserving order. Use
  `--body-renderer-arg=--option` for an argument beginning with a dash. Arguments
  without an executable are rejected.

Neither an item body nor any consumer frontmatter value can configure the
executable or arguments. The process inherits the operator's environment and
runs with the operator's authority: only configure trusted installed renderers.
No shell is used to interpret the argument vector.

### Renderer protocol

For each non-retired record in the immutable snapshot, the renderer receives one
UTF-8 JSON document on stdin: the supported `Myque.Api.itemApiValue` serialization
used by `myque api get UUID` (`api: "myque/v2"`). It includes the canonical UUID,
schema, title, state, opaque body **after the title**, complete raw consumer YAML
entries, exact-byte revision and readiness. myque-gh does not parse consumer
entries or implement Zutai or devloop semantics.

Return readable UTF-8 Markdown on stdout and exit zero. Diagnostics belong on
stderr. Nonzero exit, inability to start, invalid UTF-8, empty output for a
nonempty source description, reserved identity markers, or a description starting
with a `zt`/`zti` code fence is a local configuration failure (exit 1),
never a raw-body fallback. Rendering completes before GitHub discovery or any
remote mutation. All non-retired records are validated/rendered, including those
outside the first-projection query, because existing remote projections and
parent closure may retain them. Library planners also refuse raw structured
fences before constructing a mutation plan.

Without a renderer, legacy Markdown is preserved byte-for-byte as before.
A body beginning with a Zutai fence requires a readable renderer. Incidental
code examples inside legacy prose remain unchanged. This is a presentation
safeguard, not a body-profile validator.
With `devloop render`, devloop owns recorded-profile/schema/helper validation and
inert data decoding; unknown or malformed profiles must fail. The renderer must
handle mixed stores deliberately: legacy items without a devloop record pass
through their API body unchanged; a devloop record with an unknown profile is
not legacy. Projection must never evaluate stored requirements.

The generated identity header, state metadata, relationship section and PR links
remain owned by myque-gh; renderer output replaces only the human description
in issues and milestones. A renderer must not emit myque identity markers.

## Migration, retirement and recovery

Use MyQue's explicit migration operation. Migration changes only the envelope;
myque-gh does not infer structured requirements or insert consumer metadata.
There is no projection identity migration: the same UUID retains its issue,
milestone and N:M PR trailer associations across v1/v2, retirement and reopening.
Older clients must not rewrite v2.

Source loading reads only the resolved immutable Git commit, including
`.tasks/terminal` records, then invokes MyQue's official store loader and
validation. It does not parse terminal schemas or consult dirty working-tree
files. An all-retired checkout may omit the active items directory. Symlink or
non-regular terminal entries are refused. Missing, corrupt and unknown identities
are not silently treated as historical success.

Retired records are resolved through MyQue's combined store index, preserving
incoming/outgoing edges and done-only dependency readiness. `done` issues close
as completed; `cancelled` issues close as not planned. Reopening restores the
same UUID and reopens its existing projection. Already projected terminal
identities remain projected even outside the selection query; retirement does
not create new issues for previously unprojected completed work.

Retired descriptions explicitly state that the full body is unavailable offline;
they never fetch history or run a renderer over terminal data. They show the
retained repository identity, full commit, original path and exact-byte SHA-256.
With `--source-repo` configured to that canonical repository, the history link
uses the retained commit, never a mutable branch or a removed live-item path.
Without it, the full recovery reference remains visible as text. Relationship
fallback links use the same immutable reference. Use MyQue to retrieve and
verify history before reopening; unavailable history or a digest mismatch must
leave the item retired. Retain the original repository roots and MyQue's retained
Git references when mirroring/backing up. Retirement reduces the active checkout,
not Git history; the terminal identity index continues growing.

## Verification and releases

Run after integrating the pinned MyQue dependency:

```sh
cabal test all
nix flake check
```

The suite covers legacy preservation, readable replacement, literal argument
vectors, renderer refusal, terminal-only immutable snapshots, retired PR
resolution, closure reasons, dependency readiness and UUID reopening identity.
Use the standalone devloop integration suite to verify actual schema validation
and readable rendering; myque-gh intentionally contains no copy of those rules.
The hosted smoke executable requires a dedicated GitHub repository and authorized
credentials; consult `myque-gh-hosted-smoke --help` before running it.

Publish source/release artifacts to `github.com/mozufu/myque-gh` only after the
MyQue revision pins and supported distribution have been exercised. No release
or hosted-mutation claim follows merely from a local build.
