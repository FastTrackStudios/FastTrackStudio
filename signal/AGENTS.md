# Agent Instructions

## btca — Source Code Search

Use **btca** to query the actual source code of key dependencies before implementing features or debugging.

```bash
btca ask -r <resource> -q "your question"
btca resources   # list all available resources
```

### Relevant Resources

| Resource | Repo | Description |
|----------|------|-------------|
| `facet` | facet-rs/facet | Rust reflection — shapes, derive macros, serialization |
| `vox` | bearcove/vox | RPC service framework — service traits, streaming, SHM |
| `moire` | bearcove/moire | Instrumentation — task spawning, sync primitives |
