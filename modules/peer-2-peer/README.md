# Iroh Mesh Example

A simple example demonstrating iroh peer-to-peer networking following the quickstart guide.

## What it demonstrates

- **Endpoints**: Creating an iroh endpoint that manages P2P connections
- **Router**: Using Router to handle the accept loop and route connections by ALPN
- **Protocols**: Implementing a simple echo protocol handler
- **Discovery**: Uses default DNS discovery to resolve EndpointIDs

## Key Concepts

### Endpoints
- Each endpoint has a unique `EndpointID` (Ed25519 public key)
- Endpoints maintain connections to a "home relay" for reliability
- By default, endpoints use DNS discovery to resolve EndpointIDs

### Router
- Handles the accept loop automatically
- Routes incoming connections to protocol handlers based on ALPN
- Similar to HTTP server routers, but routes by protocol instead of URL

### Protocols
- Protocols are identified by ALPN strings (e.g., `iroh-mesh/1`)
- Multiple protocols can run on the same endpoint
- This example implements a simple echo protocol

## Usage

Run the example:

```bash
cargo run --bin iroh-mesh
```

The endpoint will:
1. Create an endpoint and connect to a relay
2. Print its EndpointID and address
3. Start listening for incoming connections
4. Echo back any messages received

## Connecting to this endpoint

To connect from another instance, you'll need:
- The EndpointID (printed when starting)
- The EndpointAddr (includes relay info)

You can use `iroh::Endpoint::connect()` with the EndpointAddr to establish a connection.

## Adding Local Discovery

To enable mDNS discovery for finding peers on the same local network, add the feature flag:

```toml
[dependencies]
iroh = { version = "0.95", features = ["discovery-local-network"] }
```

Then configure the endpoint:

```rust
let mdns = iroh::discovery::mdns::MdnsDiscovery::builder();
let endpoint = Endpoint::builder()
    .discovery(mdns)
    .bind()
    .await?;
```

## Comparison with root-multi-protocol-test

This iroh example is simpler because:
- **No manual routing protocol** - iroh handles connection establishment
- **Built-in NAT traversal** - automatic hole punching via relays
- **Default discovery** - DNS discovery works out of the box
- **Router pattern** - handles accept loop automatically

The root version gives you more control but requires more setup.
