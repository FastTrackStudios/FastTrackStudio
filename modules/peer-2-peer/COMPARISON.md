# Root vs Iroh Comparison

This document compares the two implementations side-by-side.

## Architecture Differences

### Root (`root-multi-protocol-test`)
- **Routing**: Uses root's routing protocol for multi-hop routing
- **Protocols**: IPC, WebSocket, OSC (multiple transport protocols)
- **Discovery**: ZeroConf/mDNS (manual implementation)
- **NAT Traversal**: Manual (WebSocket can work, but no hole punching)
- **Architecture**: Protocol adapters + routing layer
- **Connection Model**: Links between nodes, routing protocol discovers paths

### Iroh (`iroh-multi-protocol-test`)
- **Routing**: Direct peer-to-peer connections (no routing protocol needed)
- **Protocols**: QUIC only (via iroh)
- **Discovery**: iroh's built-in MdnsDiscovery
- **NAT Traversal**: Automatic (hole punching + relay fallback)
- **Architecture**: Router + ProtocolHandler (middleware-like)
- **Connection Model**: Direct connections, automatic NAT traversal

## Code Structure Comparison

### Root Implementation
```rust
// Protocol adapters
pub trait ProtocolAdapter {
    fn start(&mut self, mq: MessageQueue) -> Result<()>;
    fn send_packet(&self, link_id: Uuid, packet: ProtocolPacket, addr: Option<&str>) -> Result<()>;
}

// Routing via root Router
let router = Router::<MultiProtocolSystem>::new(node_id);
// Manual link management
router.links.insert(link_id, Neighbour::new(neighbor));
```

### Iroh Implementation
```rust
// Protocol handler (like middleware)
impl ProtocolHandler for MeshProtocolHandler {
    async fn accept(&self, connection: Connection) -> Result<(), AcceptError> {
        // Handle connection
    }
}

// Router handles accepting automatically
let router = Router::builder(endpoint)
    .accept(ALPN, handler)
    .spawn();
```

## Key Differences

### 1. Protocol Flexibility
- **Root**: ✅ Multiple protocols (IPC, WebSocket, OSC)
- **Iroh**: ❌ QUIC only (but can tunnel other protocols over QUIC)

### 2. Multi-hop Routing
- **Root**: ✅ Automatic routing through intermediate nodes
- **Iroh**: ❌ Direct connections only (need to implement routing yourself)

### 3. NAT Traversal
- **Root**: ❌ Manual (WebSocket can work, but no hole punching)
- **Iroh**: ✅ Automatic (hole punching + relay fallback)

### 4. Discovery
- **Root**: Manual ZeroConf implementation
- **Iroh**: Built-in MdnsDiscovery

### 5. Encryption
- **Root**: Manual (need to implement per protocol)
- **Iroh**: ✅ Built-in (TLS/QUIC)

### 6. Browser Support
- **Root**: ❌ No WASM support
- **Iroh**: ✅ WASM support for browsers

## When to Use Which?

### Use Root When:
- You need multiple transport protocols (IPC, WebSocket, OSC)
- You need multi-hop routing
- You're building a routing mesh network
- You want protocol flexibility

### Use Iroh When:
- You need NAT traversal (WAN connectivity)
- You want browser support (WASM)
- You want built-in encryption
- You're building direct peer-to-peer connections
- You want automatic relay fallback

## Hybrid Approach

You can use both:
- **Root** for local network multi-protocol routing
- **Iroh** as another protocol adapter in root for WAN connectivity

This gives you:
- Multi-protocol support (root)
- NAT traversal (iroh)
- Best of both worlds

