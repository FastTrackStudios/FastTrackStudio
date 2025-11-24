//! Macros for declaratively defining sorting groups

/// Macro to define a sorting group declaratively
/// 
/// Reduces boilerplate by defining the group struct and all three trait methods at once.
/// 
/// Example:
/// ```rust
/// define_group!(Drums, "DRUMS", "Drums", "D", {
///     add_child_by_name("Kick");
///     add_child_by_name("Snare");
/// });
/// 
/// define_group!(Bass, "BASS", "Bass", "Bass");
/// ```
#[macro_export]
macro_rules! define_group {
    // Group with children (closure)
    ($name:ident, $id:literal, $display_name:literal, $prefix:literal, { $($child:stmt)* }) => {
        pub struct $name;
        
        impl $crate::SortingGroup for $name {
            fn build() -> $crate::SortingGroupData {
                let mut group = $crate::SortingGroupData::new($id, $display_name);
                group.prefix = $prefix.to_string();
                $($child)*
                group
            }
            
            fn id() -> &'static str {
                $id
            }
            
            fn name() -> &'static str {
                $display_name
            }
        }
    };
    
    // Group without children
    ($name:ident, $id:literal, $display_name:literal, $prefix:literal) => {
        define_group!($name, $id, $display_name, $prefix, {});
    };
}

