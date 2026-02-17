# NanoC Design Document

**Version:** 0.1.0
**Last Updated:** 2026-02-17

This document describes the design philosophy, architecture, and implementation details of NanoC for contributors.

## What is NanoC?

NanoC is a minimal systems programming language targeting RISC-V. It emphasizes:

- **Simplicity**: Small, predictable language with no hidden behavior
- **Transparency**: Almost 1:1 mapping to assembly
- **Explicitness**: No implicit conversions or hidden allocations
- **No Runtime**: Direct syscalls, no libc dependency

## Who This Book Is For

This book is for:
- Language designers interested in minimal language design
- Compiler contributors working on NanoC
- Systems programmers evaluating NanoC for their projects
- Anyone curious about simple, explicit programming languages
