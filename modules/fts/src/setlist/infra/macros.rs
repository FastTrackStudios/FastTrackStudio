//! Macros for generating protocol routes from SetlistAPI trait
//!
//! This module provides macros that can automatically generate HTTP, OSC, and other
//! protocol routes from the SetlistAPI trait methods.

/// Generate protocol routes from SetlistAPI trait
///
/// This macro generates route handlers for multiple protocols (HTTP, OSC, etc.)
/// based on the SetlistAPI trait methods.
///
/// # Example
/// ```rust,ignore
/// setlist_api_routes! {
///     api: DioxusSetlistAPI,
///     base_path: "/setlist",
///     routes: {
///         http: {
///             get_setlist_name => "/name",
///             get_active_song_name => "/activeSongName",
///             get_next_song_name => "/nextSongName",
///         },
///         osc: {
///             get_setlist_name => "/setlist/name",
///             get_active_song_name => "/setlist/activeSongName",
///             get_next_song_name => "/setlist/nextSongName",
///         },
///     }
/// }
/// ```
#[macro_export]
macro_rules! setlist_api_routes {
    (
        api: $api_type:ty,
        base_path: $base:literal,
        routes: {
            http: {
                $( $http_method:ident => $http_path:literal ),* $(,)?
            },
            osc: {
                $( $osc_method:ident => $osc_path:literal ),* $(,)?
            }
        }
    ) => {
        // HTTP route generation would go here
        // OSC route generation would go here
        // This is a placeholder for the actual implementation
    };
}

/// Generate all routes from SetlistAPI trait automatically
///
/// This macro automatically generates routes for all SetlistAPI methods
/// using a naming convention.
///
/// # Example
/// ```rust,ignore
/// auto_setlist_routes! {
///     api: DioxusSetlistAPI,
///     base_path: "/setlist",
///     protocols: [http, osc]
/// }
/// ```
///
/// This will automatically generate routes like:
/// - `get_setlist_name` → HTTP: `/setlist/name`, OSC: `/setlist/name`
/// - `get_active_song_name` → HTTP: `/setlist/activeSongName`, OSC: `/setlist/activeSongName`
/// - etc.
#[macro_export]
macro_rules! auto_setlist_routes {
    (
        api: $api_type:ty,
        base_path: $base:literal,
        protocols: [ $( $protocol:ident ),* $(,)? ]
    ) => {
        // Auto-generate routes based on method names
        // Convert snake_case to camelCase for HTTP paths
        // Keep snake_case for OSC paths
        // This is a placeholder for the actual implementation
    };
}

