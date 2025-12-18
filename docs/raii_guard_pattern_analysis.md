# RAII Guard Pattern Analysis

## What is RAII with Guards?

RAII (Resource Acquisition Is Initialization) is a pattern where:
- **Resource acquisition** happens in a constructor
- **Resource release** happens in a destructor (via `Drop` trait)
- The **guard object** mediates access to the resource
- The **borrow checker** ensures the resource can't be used after the guard is dropped

### Key Benefits:
1. **Automatic cleanup**: Resources are always released, even on panic or early return
2. **Type safety**: The borrow checker prevents use-after-free errors
3. **No manual cleanup**: No need to remember to call cleanup functions
4. **Exception safety**: Works correctly even when errors occur

### In Rust:
Rust's type system makes RAII guards particularly powerful:
- `Drop` trait for automatic cleanup
- Lifetime parameters ensure guards can't outlive resources
- `Deref` trait makes guards ergonomic to use

## Current Usage in This Codebase

### ✅ Already Using RAII Guards

1. **Mutex Guards** (`std::sync::MutexGuard`)
   - Rust's standard library already provides RAII guards for mutexes
   - Automatically unlocks when dropped
   - Used in: `modules/fts/src/smart_template/template/implementations/tom.rs`

2. **File Handles** (via standard library)
   - `File` and `BufReader`/`BufWriter` automatically close on drop
   - Used throughout the codebase for file I/O

## Opportunities for Improvement

### 🔴 High Priority: REAPER Undo Block Guard

**Location**: Throughout `apps/reaper_extension/src/`

**Problem**: 
REAPER API operations that modify the project should be wrapped in undo blocks. Currently, there's no automatic way to ensure undo blocks are properly closed, especially if an error occurs.

**RAII Guard Solution**:
```rust
use reaper_rs::Reaper;

/// RAII guard for REAPER undo blocks
/// Automatically closes the undo block when dropped
pub struct UndoBlockGuard {
    description: String,
    // Store a flag to track if we should end the block
    // (in case we want to manually end it early)
    active: bool,
}

impl UndoBlockGuard {
    /// Start a new undo block
    pub fn begin(description: impl Into<String>) -> Self {
        let description = description.into();
        let reaper = Reaper::get();
        reaper.medium_reaper().undo_begin_block(&description);
        
        Self {
            description,
            active: true,
        }
    }
    
    /// Manually end the undo block early (optional)
    pub fn end(mut self) {
        if self.active {
            let reaper = Reaper::get();
            reaper.medium_reaper().undo_end_block(&self.description);
            self.active = false;
        }
    }
}

impl Drop for UndoBlockGuard {
    fn drop(&mut self) {
        if self.active {
            let reaper = Reaper::get();
            reaper.medium_reaper().undo_end_block(&self.description);
        }
    }
}

// Usage:
pub fn create_track_with_undo(name: &str) -> Result<(), Error> {
    let _undo = UndoBlockGuard::begin("Create Track");
    
    // If any of these operations fail, the undo block is still closed
    let project = get_current_project()?;
    let track = project.create_track(name)?;
    track.set_volume(0.8)?;
    
    // Undo block automatically closed when _undo goes out of scope
    Ok(())
}
```

**Benefits**:
- Undo blocks are always properly closed
- Works even if operations fail or panic
- Clear, self-documenting code

### 🔴 High Priority: UI Refresh Prevention Guard

**Location**: `apps/reaper_extension/src/`

**Problem**: 
REAPER operations that modify many things should prevent UI refresh for performance. Currently, there's no automatic way to ensure UI refresh is re-enabled.

**RAII Guard Solution**:
```rust
/// RAII guard for preventing UI refresh
/// Automatically re-enables UI refresh when dropped
pub struct PreventUIRefreshGuard {
    // Track if we should re-enable (in case we manually re-enable early)
    active: bool,
}

impl PreventUIRefreshGuard {
    /// Prevent UI refresh
    pub fn new() -> Self {
        let reaper = Reaper::get();
        reaper.medium_reaper().prevent_ui_refresh(1);
        
        Self { active: true }
    }
    
    /// Manually re-enable UI refresh early (optional)
    pub fn re_enable(mut self) {
        if self.active {
            let reaper = Reaper::get();
            reaper.medium_reaper().prevent_ui_refresh(-1);
            self.active = false;
        }
    }
}

impl Drop for PreventUIRefreshGuard {
    fn drop(&mut self) {
        if self.active {
            let reaper = Reaper::get();
            reaper.medium_reaper().prevent_ui_refresh(-1);
        }
    }
}

// Usage:
pub fn bulk_operation() -> Result<(), Error> {
    let _ui_guard = PreventUIRefreshGuard::new();
    
    // Perform many operations...
    for i in 0..100 {
        create_track(&format!("Track {}", i))?;
    }
    
    // UI refresh automatically re-enabled when _ui_guard goes out of scope
    Ok(())
}
```

### 🟡 Medium Priority: Cursor Position Guard

**Location**: `apps/reaper_extension/src/services/seek_service.rs`, `apps/reaper_extension/src/live/tracks/actions/navigation.rs`

**Problem**: 
Multiple places set the edit cursor position. Sometimes you might want to temporarily move the cursor and restore it afterward.

**RAII Guard Solution**:
```rust
use reaper_rs::{Project, PositionInSeconds};

/// RAII guard for cursor position
/// Automatically restores the cursor position when dropped
pub struct CursorPositionGuard<'a> {
    project: &'a Project,
    original_position: PositionInSeconds,
    restore: bool,
}

impl<'a> CursorPositionGuard<'a> {
    /// Save current cursor position and optionally set a new one
    pub fn new(project: &'a Project) -> Result<Self, Error> {
        let original_position = project.edit_cursor_position();
        
        Ok(Self {
            project,
            original_position,
            restore: true,
        })
    }
    
    /// Save current position and move to a new position
    pub fn with_position(
        project: &'a Project,
        new_position: PositionInSeconds,
    ) -> Result<Self, Error> {
        let original_position = project.edit_cursor_position();
        
        project.set_edit_cursor_position(
            new_position,
            SetEditCurPosOptions { move_view: false, seek_play: false }
        );
        
        Ok(Self {
            project,
            original_position,
            restore: true,
        })
    }
    
    /// Don't restore the cursor position when dropped
    pub fn keep_position(mut self) {
        self.restore = false;
    }
}

impl<'a> Drop for CursorPositionGuard<'a> {
    fn drop(&mut self) {
        if self.restore {
            self.project.set_edit_cursor_position(
                self.original_position,
                SetEditCurPosOptions { move_view: false, seek_play: false }
            );
        }
    }
}

// Usage:
pub fn temporary_cursor_operation() -> Result<(), Error> {
    let project = get_current_project()?;
    let target_pos = PositionInSeconds::new(10.0)?;
    
    // Move cursor temporarily
    let _cursor_guard = CursorPositionGuard::with_position(&project, target_pos)?;
    
    // Do operations at the new cursor position...
    perform_operations()?;
    
    // Cursor automatically restored to original position
    Ok(())
}
```

### 🟡 Medium Priority: Mutex Lock Scoping Improvement

**Location**: `modules/fts/src/smart_template/template/implementations/tom.rs`

**Current Pattern** (lines 132-135, 147-157):
```rust
let mapping_result = {
    let mut mapper = self.mapper.lock().unwrap();
    mapper.map_track_name(track_name)
};

// ... other code ...

let mut template = self.template.lock().unwrap();
// Use template...
```

**Observation**: 
The current code already uses RAII correctly! The mutex guards are automatically released when they go out of scope. However, the pattern could be made more explicit with helper methods.

**Potential Enhancement**:
```rust
impl TomMatcher {
    /// Execute a closure with the mapper locked
    pub fn with_mapper<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&mut TomMapper) -> R,
    {
        let mut mapper = self.mapper.lock().unwrap();
        f(&mut mapper)
    }
    
    /// Execute a closure with the template locked
    pub fn with_template<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&mut Template) -> R,
    {
        let mut template = self.template.lock().unwrap();
        f(&mut template)
    }
}

// Usage:
let mapping_result = matcher.with_mapper(|mapper| {
    mapper.map_track_name(track_name)
});

matcher.with_template(|template| {
    if !template.tracks.iter().any(|t| t.name == display_track_name) {
        template.tracks.push(create_track(...));
    }
});
```

**Benefits**:
- More explicit about lock scoping
- Prevents accidentally holding locks too long
- Makes the RAII nature more obvious

### 🟢 Low Priority: Transaction Guard

**Location**: Anywhere multiple operations need to be atomic

**Use Case**: 
When you need to perform multiple operations that should all succeed or all fail together.

**RAII Guard Solution**:
```rust
/// RAII guard for a transaction
/// Automatically rolls back if not committed
pub struct TransactionGuard {
    committed: bool,
    rollback_fn: Box<dyn FnOnce()>,
}

impl TransactionGuard {
    pub fn new(rollback_fn: impl FnOnce() + 'static) -> Self {
        Self {
            committed: false,
            rollback_fn: Box::new(rollback_fn),
        }
    }
    
    pub fn commit(mut self) {
        self.committed = true;
    }
}

impl Drop for TransactionGuard {
    fn drop(&mut self) {
        if !self.committed {
            let rollback = std::mem::replace(&mut self.rollback_fn, Box::new(|| {}));
            rollback();
        }
    }
}

// Usage:
pub fn atomic_operation() -> Result<(), Error> {
    let original_state = save_current_state();
    
    let transaction = TransactionGuard::new(move || {
        restore_state(original_state);
    });
    
    // Perform operations...
    operation1()?;
    operation2()?;
    operation3()?;
    
    // If we get here, commit the transaction
    transaction.commit();
    Ok(())
    
    // If any operation fails, the transaction guard automatically rolls back
}
```

## Recommendations

### Priority 1: Implement Undo Block Guard
This is the most important because:
- REAPER operations should be undoable
- Easy to forget to close undo blocks
- Critical for user experience

### Priority 2: Implement UI Refresh Guard
Important for performance when doing bulk operations.

### Priority 3: Consider Cursor Position Guard
Useful for operations that temporarily move the cursor.

### Priority 4: Enhance Mutex Usage Patterns
The current usage is already correct, but helper methods could make it more explicit.

## When NOT to Use RAII Guards

Don't use RAII guards when:
- The resource cleanup is trivial and the guard adds unnecessary complexity
- You need explicit control over when cleanup happens (though you can still use guards with manual `drop()`)
- The guard would hold references that prevent needed operations
- Performance is critical and the guard adds overhead (rare in Rust)

## Summary

Your codebase already benefits from Rust's built-in RAII:
- `MutexGuard` automatically unlocks mutexes
- File handles automatically close
- Memory is automatically freed

The main opportunities are:
1. **REAPER-specific guards** (undo blocks, UI refresh)
2. **Application-specific guards** (cursor position, transactions)
3. **Pattern improvements** (making RAII usage more explicit)

RAII guards would make your code more robust, especially in error scenarios, and would prevent resource leaks in REAPER operations.
