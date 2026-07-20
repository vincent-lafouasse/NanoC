# ADR-0006: Array bounds checking

**Status:** Settled (UB)
**Area:** Language Design

## Context

Out-of-bounds array access is undefined behavior in C. Whether NanoC keeps that, or defines
it, is open — ties into the broader "Defined Behavior" pattern in the Tour's Language
Design chapter, where NanoC deliberately makes several C-UB cases well-defined when doing
so serves no optimization purpose. Bounds checking is different: unlike signed overflow or
shift semantics, catching an out-of-bounds access has a real runtime cost.

## Options

1. **Leave as UB** — matches C, zero runtime cost, but the hazard is silent.
2. **Trap/panic** — safe, but a runtime check on every array access, in tension with the
   "1:1 mapping to assembly" philosophy (every line should correspond to something the
   processor does — a bounds check is *additional* something).
3. **Return zero** — defined, but silently wrong in a way that's arguably worse than UB: a
   masked bug that produces plausible-looking garbage instead of a crash.

## Decision

UB, same as C. Settled, not just a placeholder default: anything other than UB forces a
runtime cost onto every array access for everyone, including the programmers who don't
need the check and already know what they're doing. It's your gun, your foot, and NanoC
will let you shoot it — consistent with the rest of the "Defined Behavior" table's
philosophy of only paying for a guarantee when it costs nothing to give.

May still add *optional* bounds checking later (a compiler flag, or an explicit "checked
index" operation) — but as an opt-in on top of UB-by-default, not a replacement for it.
Since arrays aren't implemented at all yet, this is somewhat moot until then anyway.

## History

- Predates version tracking — carried over from the original design document. Originally
  recorded as open/current-default; corrected to settled — this isn't a placeholder, it's
  the actual decision.
