# PRD: FTS Allocator — Real-Time-Aware Runtime for REAPER Extensions

## Problem

The REAPER extension has several scattered threading/allocation mechanisms:
- `TaskSupport` + `MainTaskMiddleware` (from reaper-high) for main-thread dispatch
- `Global` singleton wrapping crossbeam channels
- Timer callback manually polling tasks
- No real-time thread detection or allocation safety
- No async deallocation (RT threads can trigger allocator in destructors)

These are functional but fragile. Helgobox's allocator pattern
(https://github.com/helgoboss/helgobox) demonstrates a superior approach:
automatic RT thread detection, async deallocation offloading, and RAII guards.

## Goal

A unified `fts-allocator` crate at `crates/shared/fts-allocator` that:
1. Replaces the global allocator with an RT-aware version (Helgobox pattern)
2. Provides main-thread task dispatch (replacing TaskSupport/Global)
3. Integrates with moire for dashboard visibility
4. Provides RAII guards for no-alloc zones

## User Stories

### US-001: RT-Aware Global Allocator
Create a custom `GlobalAlloc` implementation inspired by Helgobox's
`HelgobossAllocator`. Detects when deallocation happens on a registered
RT thread and offloads it to a dedicated background thread via bounded channel.

### US-002: Main-Thread Task Dispatch
Replace `TaskSupport`/`Global`/`MainTaskMiddleware` with a cleaner API:
- `FtsRuntime::on_main_thread(f)` → queues closure, returns oneshot receiver
- `FtsRuntime::do_on_main_thread(f)` → fire-and-forget
- Timer callback calls `FtsRuntime::process_main_thread_tasks()`
- All channels are moire-instrumented with names

### US-003: RT Thread Registration
Provide API for registering threads as real-time:
- `FtsRuntime::register_rt_thread()` — marks current thread
- `FtsRuntime::is_rt_thread()` → bool
- Used by the allocator to decide offloading

### US-004: RAII Guards
- `assert_no_alloc(|| { ... })` — panics in debug if allocation happens
- `permit_alloc(|| { ... })` — temporarily allows allocation inside no-alloc zone
- Guards use thread-local counters (Helgobox pattern)

### US-005: Foreign Value Deallocation
Support deallocation of C/FFI values on the background thread:
- `FtsRuntime::dealloc_foreign(ptr, deallocate_fn)` — offloads C dealloc
- Essential for REAPER plugin interop where C objects are freed from RT context
