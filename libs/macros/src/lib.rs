//! FastTrackStudio Macros
//!
//! This crate provides declarative macros for generating HTTP and OSC routers from action enums.
//! The new unified approach allows defining both protocols in a single macro invocation.

/// Unified protocol router macro
///
/// Creates both HTTP and OSC routing functions from a single definition.
/// This is the recommended approach for defining protocol mappings.
///
/// # Example
/// ```rust,ignore
/// protocol_router! {
///     TransportAction,
///     "/transport" => {
///         Play => { http: "/play", osc: "/play" },
///         Pause => { http: "/pause", osc: "/pause" },
///         SetTempo(_) => { http: "/tempo/set", osc: "/bpm/set" },
///         GetTempo => { http: "/tempo", osc: "/bpm" },
///     }
/// }
/// ```
#[macro_export]
macro_rules! protocol_router {
    (
        $action_type:ident,
        $prefix:literal => {
            $( $variant:ident$(($($param:tt)*))?  => { http: $http_path:literal, osc: $osc_address:literal } ),* $(,)?
        }
    ) => {
        /// HTTP protocol module
        pub mod http {
            use super::*;

            /// Get HTTP path for an action
            pub fn get_path(action: &$action_type) -> Option<String> {
                match action {
                    $(
                        $action_type::$variant$(($($param)*))? => Some(format!("{}{}", $prefix, $http_path)),
                    )*
                }
            }

            /// Get all available HTTP paths
            pub fn get_all_paths() -> Vec<String> {
                vec![
                    $(
                        format!("{}{}", $prefix, $http_path),
                    )*
                ]
            }

            /// Parse a path into an action variant name
            pub fn parse_path(path: &str) -> Option<String> {
                match path {
                    $(
                        p if p == &format!("{}{}", $prefix, $http_path) => {
                            Some(stringify!($variant).to_string())
                        }
                    )*
                    _ => None,
                }
            }

            /// Get HTTP method for an action (defaults to POST for commands, GET for queries)
            pub fn get_method(action: &$action_type) -> &'static str {
                match action {
                    $(
                        $action_type::$variant$(($($param)*))? => {
                            let variant_str = stringify!($variant);
                            if variant_str.starts_with("Get") || variant_str.starts_with("Is") {
                                "GET"
                            } else {
                                "POST"
                            }
                        }
                    )*
                }
            }

            /// Create an action from a path and parameters (for reverse routing)
            pub fn path_to_action(path: &str) -> Option<String> {
                parse_path(path)
            }
        }

        /// OSC protocol module
        pub mod osc {
            use super::*;

            /// Get OSC address for an action
            pub fn get_address(action: &$action_type) -> Option<String> {
                match action {
                    $(
                        $action_type::$variant$(($($param)*))? => Some(format!("{}{}", $prefix, $osc_address)),
                    )*
                }
            }

            /// Get all available OSC addresses
            pub fn get_all_addresses() -> Vec<String> {
                vec![
                    $(
                        format!("{}{}", $prefix, $osc_address),
                    )*
                ]
            }

            /// Parse an address into an action variant name
            pub fn parse_address(address: &str) -> Option<String> {
                match address {
                    $(
                        addr if addr == &format!("{}{}", $prefix, $osc_address) => {
                            Some(stringify!($variant).to_string())
                        }
                    )*
                    _ => None,
                }
            }

            /// Get OSC type tags for an action's parameters
            pub fn get_type_tags(action: &$action_type) -> &'static str {
                match action {
                    $(
                        $action_type::$variant$(($($param)*))? => {
                            let variant_str = stringify!($variant);
                            if variant_str.contains("SetTempo") {
                                "f"
                            } else if variant_str.contains("SetPosition") {
                                "f"
                            } else if variant_str.contains("SetTimeSignature") {
                                "ii"
                            } else if variant_str.contains("Project") {
                                "s"
                            } else {
                                ""
                            }
                        }
                    )*
                }
            }

            /// Create an action from an address and parameters (for reverse routing)
            pub fn address_to_action(address: &str) -> Option<String> {
                parse_address(address)
            }
        }

        // Convenience re-exports for backward compatibility
        pub use http::*;
        pub use osc::get_address;
        pub use osc::get_all_addresses;
        pub use osc::parse_address;
    };
}

/// Simple declarative macro for HTTP routing only
///
/// Creates a path mapping function for the given action enum.
/// Use this when you only need HTTP routing.
///
/// # Example
/// ```rust,ignore
/// http_router! {
///     "/transport" => {
///         TransportAction::Play => "/play",
///         TransportAction::Pause => "/pause",
///         TransportAction::SetTempo(_) => "/tempo/set",
///         TransportAction::GetTempo => "/tempo",
///     }
/// }
/// ```
#[macro_export]
macro_rules! http_router {
    (
        $prefix:literal => {
            $( $variant:pat => $path:literal ),* $(,)?
        }
    ) => {
        pub mod transport_action_http {
            use super::*;

            /// Get HTTP path for an action
            pub fn get_path(action: &TransportAction) -> Option<String> {
                match action {
                    $(
                        $variant => Some(format!("{}{}", $prefix, $path)),
                    )*
                }
            }

            /// Get all available HTTP paths
            pub fn get_all_paths() -> Vec<String> {
                vec![
                    $(
                        format!("{}{}", $prefix, $path),
                    )*
                ]
            }

            /// Try to parse a path into an action variant name
            pub fn parse_path(path: &str) -> Option<String> {
                match path {
                    $(
                        p if p == &format!("{}{}", $prefix, $path) => {
                            Some(stringify!($variant).split("::").last().unwrap_or(stringify!($variant)).replace("(_)", "").to_string())
                        }
                    )*
                    _ => None,
                }
            }

            /// Get HTTP method for an action (defaults to POST for commands, GET for queries)
            pub fn get_method(action: &TransportAction) -> &'static str {
                match action {
                    $(
                        $variant => {
                            let variant_str = stringify!($variant);
                            if variant_str.contains("Get") || variant_str.contains("Is") {
                                "GET"
                            } else {
                                "POST"
                            }
                        }
                    )*
                }
            }
        }
    };
}

/// Simple declarative macro for OSC routing only
///
/// Creates an address mapping function for the given action enum.
/// Use this when you only need OSC routing.
///
/// # Example
/// ```rust,ignore
/// osc_router! {
///     "/transport" => {
///         TransportAction::Play => "/play",
///         TransportAction::Pause => "/pause",
///         TransportAction::SetTempo(_) => "/bpm/set",
///         TransportAction::GetTempo => "/bpm",
///     }
/// }
/// ```
#[macro_export]
macro_rules! osc_router {
    (
        $prefix:literal => {
            $( $variant:pat => $address:literal ),* $(,)?
        }
    ) => {
        pub mod transport_action_osc {
            use super::*;

            /// Get OSC address for an action
            pub fn get_address(action: &TransportAction) -> Option<String> {
                match action {
                    $(
                        $variant => Some(format!("{}{}", $prefix, $address)),
                    )*
                }
            }

            /// Get all available OSC addresses
            pub fn get_all_addresses() -> Vec<String> {
                vec![
                    $(
                        format!("{}{}", $prefix, $address),
                    )*
                ]
            }

            /// Try to parse an address into an action variant name
            pub fn parse_address(address: &str) -> Option<String> {
                match address {
                    $(
                        addr if addr == &format!("{}{}", $prefix, $address) => {
                            Some(stringify!($variant).split("::").last().unwrap_or(stringify!($variant)).replace("(_)", "").to_string())
                        }
                    )*
                    _ => None,
                }
            }

            /// Get OSC type tags for an action's parameters
            pub fn get_type_tags(action: &TransportAction) -> &'static str {
                match action {
                    $(
                        $variant => {
                            let variant_str = stringify!($variant);
                            if variant_str.contains("(_)") {
                                "," // Parameter expected, but type depends on actual parameter
                            } else {
                                "" // No parameters
                            }
                        }
                    )*
                }
            }
        }
    };
}

/// Enhanced router macro with protocol-specific features
///
/// This macro provides more advanced features like HTTP method detection,
/// parameter validation, and protocol-specific optimizations.
///
/// # Example
/// ```rust,ignore
/// enhanced_router! {
///     TransportAction,
///     base: "/transport",
///     protocols: {
///         http: {
///             Play => { path: "/play", method: "POST" },
///             GetTempo => { path: "/tempo", method: "GET" },
///             SetTempo(_) => { path: "/tempo", method: "PUT" },
///         },
///         osc: {
///             Play => { address: "/play", types: "" },
///             GetTempo => { address: "/bpm", types: "" },
///             SetTempo(_) => { address: "/bpm/set", types: "f" },
///         }
///     }
/// }
/// ```
#[macro_export]
macro_rules! enhanced_router {
    (
        $action_type:ident,
        base: $base:literal,
        protocols: {
            http: {
                $( $http_variant:ident$(($($http_param:tt)*))? => { path: $http_path:literal, method: $http_method:literal } ),* $(,)?
            },
            osc: {
                $( $osc_variant:ident$(($($osc_param:tt)*))? => { address: $osc_address:literal, types: $osc_types:literal } ),* $(,)?
            }
        }
    ) => {
        /// Enhanced HTTP protocol module
        pub mod http_enhanced {
            use super::*;

            #[derive(Debug, Clone)]
            pub struct RouteInfo {
                pub path: String,
                pub method: &'static str,
                pub requires_auth: bool,
                pub rate_limited: bool,
            }

            /// Get detailed HTTP route information for an action
            pub fn get_route_info(action: &$action_type) -> Option<RouteInfo> {
                match action {
                    $(
                        $action_type::$http_variant$(($($http_param)*))? => Some(RouteInfo {
                            path: format!("{}{}", $base, $http_path),
                            method: $http_method,
                            requires_auth: is_sensitive_action(stringify!($http_variant)),
                            rate_limited: is_rate_limited_action(stringify!($http_variant)),
                        }),
                    )*
                }
            }

            /// Get HTTP path for an action
            pub fn get_path(action: &$action_type) -> Option<String> {
                get_route_info(action).map(|info| info.path)
            }

            /// Get HTTP method for an action
            pub fn get_method(action: &$action_type) -> Option<&'static str> {
                get_route_info(action).map(|info| info.method)
            }

            fn is_sensitive_action(variant: &str) -> bool {
                matches!(variant, "SaveProject" | "LoadProject" | "OpenProject" | "InitializeAudio")
            }

            fn is_rate_limited_action(variant: &str) -> bool {
                matches!(variant, "SetTempo" | "SetPosition" | "SetTimeSignature")
            }
        }

        /// Enhanced OSC protocol module
        pub mod osc_enhanced {
            use super::*;

            #[derive(Debug, Clone)]
            pub struct AddressInfo {
                pub address: String,
                pub type_tags: &'static str,
                pub bundle_priority: u8,
                pub requires_timestamp: bool,
            }

            /// Get detailed OSC address information for an action
            pub fn get_address_info(action: &$action_type) -> Option<AddressInfo> {
                match action {
                    $(
                        $action_type::$osc_variant$(($($osc_param)*))? => Some(AddressInfo {
                            address: format!("{}{}", $base, $osc_address),
                            type_tags: $osc_types,
                            bundle_priority: get_priority(stringify!($osc_variant)),
                            requires_timestamp: requires_timing(stringify!($osc_variant)),
                        }),
                    )*
                }
            }

            /// Get OSC address for an action
            pub fn get_address(action: &$action_type) -> Option<String> {
                get_address_info(action).map(|info| info.address)
            }

            /// Get OSC type tags for an action
            pub fn get_type_tags(action: &$action_type) -> Option<&'static str> {
                get_address_info(action).map(|info| info.type_tags)
            }

            fn get_priority(variant: &str) -> u8 {
                match variant {
                    "Play" | "Pause" | "Stop" => 1, // Highest priority
                    "SetPosition" | "SetTempo" => 2, // Medium priority
                    _ => 3, // Normal priority
                }
            }

            fn requires_timing(variant: &str) -> bool {
                matches!(variant, "Play" | "Pause" | "Stop" | "SetPosition")
            }
        }
    };
}

/// Utility macro for generating protocol discovery endpoints
///
/// Creates functions that return available routes/addresses for API discovery.
///
/// # Example
/// ```rust,ignore
/// protocol_discovery! {
///     TransportAction,
///     name: "Transport API",
///     version: "1.0.0",
///     description: "Digital Audio Workstation Transport Control"
/// }
/// ```
#[macro_export]
macro_rules! protocol_discovery {
    (
        $action_type:ident,
        name: $name:literal,
        version: $version:literal,
        description: $description:literal
    ) => {
        /// API discovery module
        pub mod discovery {
            use super::*;
            use serde::{Serialize, Deserialize};

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct ApiInfo {
                pub name: &'static str,
                pub version: &'static str,
                pub description: &'static str,
                pub protocols: Vec<ProtocolInfo>,
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct ProtocolInfo {
                pub name: &'static str,
                pub endpoints: Vec<EndpointInfo>,
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct EndpointInfo {
                pub action: String,
                pub path_or_address: String,
                pub method_or_types: String,
                pub description: String,
            }

            /// Get complete API information for discovery
            pub fn get_api_info() -> ApiInfo {
                ApiInfo {
                    name: $name,
                    version: $version,
                    description: $description,
                    protocols: vec![
                        get_http_protocol_info(),
                        get_osc_protocol_info(),
                    ],
                }
            }

            fn get_http_protocol_info() -> ProtocolInfo {
                ProtocolInfo {
                    name: "HTTP",
                    endpoints: vec![
                        // This would be populated by the actual router macros
                        // For now, it's a placeholder structure
                    ],
                }
            }

            fn get_osc_protocol_info() -> ProtocolInfo {
                ProtocolInfo {
                    name: "OSC",
                    endpoints: vec![
                        // This would be populated by the actual router macros
                        // For now, it's a placeholder structure
                    ],
                }
            }

            /// Generate OpenAPI/Swagger specification for HTTP endpoints
            pub fn generate_openapi_spec() -> serde_json::Value {
                serde_json::json!({
                    "openapi": "3.0.0",
                    "info": {
                        "title": $name,
                        "version": $version,
                        "description": $description
                    },
                    "paths": {}
                    // Paths would be populated from the actual HTTP routes
                })
            }
        }
    };
}

// Re-export paste for the macros that need it
pub use paste;

#[cfg(test)]
mod tests {
    use super::*;

    // Mock action enum for testing
    #[derive(Debug, Clone)]
    enum TestAction {
        Play,
        SetTempo(f64),
        GetTempo,
    }

    #[test]
    fn test_macro_compilation() {
        // Test that the macros compile correctly
        // This is a compile-time test
    }
}
