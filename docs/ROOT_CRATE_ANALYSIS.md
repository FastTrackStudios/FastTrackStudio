# Root Crate Analysis

## Overview

**root** is an I/O-free routing framework inspired by the [Babel Routing Protocol (RFC 8966)](https://datatracker.ietf.org/doc/html/rfc8966). It provides a high-level, platform-agnostic framework for building dynamic, fault-tolerant mesh networks.

**Key Philosophy**: The application is solely responsible for I/O and scheduling events, meaning users can use any platform, framework, or architecture they desire.

## Core Architecture

### I/O-Free Design

- **No network stack dependencies**: Works with any transport (TCP, UDP, Unix sockets, Bluetooth, Serial, etc.)
- **Protocol agnostic**: Can route over multiple protocols simultaneously (IPv4, IPv6, Unix sockets, Bluetooth, UDP)
- **Deterministic**: Thoroughly testable with deterministic state at all times
- **Application-controlled**: All I/O decisions are left to the application layer

### Bidirectional Links (Conceptual)

The README mentions "bidirectional links", but this is **conceptual**, not a strict protocol requirement:

- **What it means**: You must be able to send packets TO a neighbor AND receive packets FROM that neighbor
- **UDP support**: ✅ UDP works! You can send UDP datagrams to an address and receive UDP datagrams from that address
- **Unidirectional protocols**: Would need to be modeled as two separate links (one for send, one for receive)
- **Packet loss**: The Babel protocol (which root implements) is designed for unreliable networks and handles packet loss through periodic updates and sequence numbers

### Key Concepts

#### 1. **RoutingSystem Trait**
A compile-time configuration that defines your network parameters:

```rust
pub trait RoutingSystem {
    type NodeAddress: RootData + RootKey;  // Globally unique node identifier
    type Link: RootData + RootKey;          // Physical interface identifier
    type MACSystem: MACSystem<Self>;        // Message authentication system
}
```

#### 2. **Router**
The core routing engine that maintains:
- **Links**: Direct bidirectional connections to neighbors
- **Routes**: Routing table with metrics, next hops, and feasibility distances
- **Sequence numbers**: For tracking route freshness and preventing loops
- **Outbound packets**: Queue of routing packets to send

#### 3. **Packet Types**
- `UrgentRouteUpdate`: Immediate route updates (retractions, seqno changes)
- `BatchRouteUpdate`: Periodic full-table updates
- `SeqnoRequest`: Request for updated sequence number when route is starved

#### 4. **Route Selection Algorithm**
Based on Babel's feasibility condition:
- Uses **feasibility distance (fd)** to prevent routing loops
- Compares sequence numbers to determine route freshness
- Automatically retracts routes when links fail
- Handles starvation (routes with INF metric) by requesting updates

## Features

### 1. **Automatic Route Discovery**
- Nodes automatically discover routes through neighbor advertisements
- Routes converge through iterative updates
- Supports multi-hop routing (A → B → C)

### 2. **Fault Tolerance**
- Automatic route retraction on link failure
- Sequence number tracking prevents stale routes
- Starvation detection and recovery
- Link health monitoring (in examples)

### 3. **Multi-Protocol Support**
- Can route over multiple transport protocols simultaneously
- Each link can use a different protocol (Unix socket, TCP, UDP, WebSocket, etc.)
- Application decides how to serialize/deserialize packets
- **UDP support**: Yes! UDP works with root. The "bidirectional" requirement is conceptual (send TO and receive FROM a neighbor), which UDP supports via datagrams

### 4. **Message Authentication (MAC)**
- Pluggable MAC system for packet signing/validation
- `NoMACSystem` for development/testing (trusts all packets)
- Can implement custom MAC for production use

### 5. **Sequence Number Management**
- Prevents routing loops
- Tracks route freshness
- Handles resynchronization when nodes wake from sleep
- Prevents amplification attacks through deduplication

## API Overview

### Creating a Router

```rust
struct MyNetwork {}
impl RoutingSystem for MyNetwork {
    type NodeAddress = String;
    type Link = Uuid;
    type MACSystem = NoMACSystem;
}

let mut router = Router::<MyNetwork>::new("node-id".to_string());
```

### Adding Links

```rust
router.links.insert(
    link_id,
    Neighbour {
        addr: "neighbor-id".to_string(),
        metric: 1,  // Link cost (lower is better)
        routes: HashMap::new(),
    }
);
```

### Processing Updates

```rust
// Handle incoming packet
router.handle_packet(&packet, &link_id, &neighbor_addr)?;

// Update routing table
router.update();  // Updates routes, doesn't broadcast
// OR
router.full_update();  // Updates routes AND broadcasts to neighbors
```

### Accessing Routes

```rust
if let Some(route) = router.routes.get(&destination) {
    let next_hop = &route.next_hop;
    let metric = route.metric;
    let link = &route.link;  // Use this to send packets
}
```

### Sending Packets

```rust
// Router generates outbound packets automatically
for packet in router.outbound_packets.drain(..) {
    // Serialize packet.data() and send via packet.link
    // Destination is packet.dest
}
```

## Example Implementation Pattern

The `simple-mesh` example shows a complete TCP-based implementation:

1. **Server Thread**: Listens for incoming TCP connections
2. **Link I/O Threads**: One per neighbor, handles bidirectional communication
3. **Packet Sender Thread**: Routes outbound packets to appropriate link threads
4. **Main Loop**: Processes events, handles packets, updates router
5. **Timer Threads**: Periodic route broadcasts and health checks

### Key Pattern:
```rust
// 1. Receive packet from network
let packet: NetPacket = deserialize(network_bytes)?;

// 2. Extract root routing packet
if let NetPacket::Routing { link_id, data } = packet {
    router.handle_packet(&data, &link_id, &neighbor)?;
    router.update();
}

// 3. Send routing updates
for outbound in router.outbound_packets.drain(..) {
    let net_packet = NetPacket::Routing {
        link_id: outbound.link,
        data: outbound.packet.data().clone(),
    };
    send_via_link(outbound.link, serialize(net_packet)?);
}
```

## Advantages for Your Use Case

### ✅ Multi-Protocol Routing
- **Unix sockets** for Reaper Extension ↔ Desktop (fast local)
- **WebSocket** for Desktop ↔ Web Clients (network)
- **WebSocket/TCP/UDP** for Desktop ↔ Desktop (mesh)
- **UDP** for low-latency, high-throughput scenarios (broadcasting state updates)

### ✅ Automatic Topology Discovery
- Nodes automatically discover routes
- No manual configuration needed
- Handles dynamic topology changes

### ✅ Fault Tolerance
- Automatic failover when links fail
- Can support multiple paths to same destination
- Handles network partitions gracefully

### ✅ I/O-Free Design
- Works with Dioxus fullstack WebSockets
- Works with Unix sockets
- Works with any transport you choose

### ✅ Deterministic & Testable
- Can test routing logic without network I/O
- Deterministic state makes debugging easier
- Can simulate network topologies

## Limitations & Considerations

### ⚠️ Learning Curve
- Requires understanding of Babel routing protocol concepts
- More complex than simple broadcaster pattern
- Need to understand feasibility conditions, sequence numbers, etc.

### ⚠️ Overhead
- More sophisticated than needed for simple master/slave
- Routing protocol overhead (sequence numbers, route updates)
- May be overkill if you only need direct connections

### ⚠️ MAC System
- Default `NoMACSystem` trusts all packets (development only)
- Need to implement proper MAC for production security
- Examples don't implement MAC (can forge routes/packets)

### ⚠️ State Management
- Router maintains its own routing state
- Need to integrate with your application state
- Route table is separate from application data

## Comparison to Alternatives

### vs. Custom Message Router (Simple Broadcaster)
**root advantages:**
- Automatic route discovery
- Multi-hop routing
- Fault tolerance
- Proven algorithm (Babel)

**Custom router advantages:**
- Simpler for direct connections
- Less overhead
- Easier to understand
- Full control

### vs. Dioxus Fullstack WebSockets Only
**root advantages:**
- Multi-protocol support (not just WebSocket)
- Automatic topology discovery
- Can route through intermediate nodes

**Dioxus advantages:**
- Already integrated
- Simpler for direct connections
- Type-safe with Dioxus signals

## Integration Strategy

### Option 1: Full Integration
Use root as the core routing layer:
1. Define `RoutingSystem` with your node addresses and links
2. Implement link adapters (Unix socket, WebSocket)
3. Router handles all routing decisions
4. Application state syncs through routed packets

### Option 2: Hybrid Approach
Use root for mesh discovery, custom routing for direct connections:
1. root discovers topology and optimal paths
2. Direct connections use simple broadcaster
3. Multi-hop uses root routing

### Option 3: Minimal Integration
Use root concepts but simpler implementation:
1. Borrow sequence number tracking
2. Use feasibility distance concepts
3. Implement simpler routing algorithm

## Recommendation

For your architecture (Reaper ↔ Desktop ↔ Web Clients + Desktop ↔ Desktop mesh):

**Start with root if:**
- You need automatic topology discovery
- You want multi-hop routing (Desktop A → Desktop B → Desktop C)
- You need fault tolerance and automatic failover
- You're building a true mesh network

**Use custom router if:**
- All connections are direct (no multi-hop)
- You want simpler implementation
- You don't need automatic topology discovery
- You prefer full control over routing logic

**Hybrid approach:**
- Use root for Desktop ↔ Desktop mesh
- Use simple broadcaster for Desktop → Web Clients (direct)
- Use Unix sockets for Reaper ↔ Desktop (direct, fast)

## Next Steps

1. **Evaluate complexity**: Is root's sophistication worth it for your use case?
2. **Prototype**: Build a simple example connecting two desktop apps
3. **Measure**: Compare overhead vs. custom solution
4. **Decide**: Choose based on actual needs (mesh vs. direct connections)

## Protocol Support Details

### Supported Protocols

| Protocol | Works? | Notes |
|----------|--------|-------|
| **TCP** | ✅ Yes | Reliable, ordered delivery. Example implementation provided |
| **UDP** | ✅ Yes | Unreliable but fast. Root handles packet loss via periodic updates |
| **Unix Sockets** | ✅ Yes | Fast local IPC. Works like TCP conceptually |
| **WebSocket** | ✅ Yes | Built on TCP, works seamlessly |
| **Serial/Bluetooth** | ✅ Yes | Any protocol where you can send/receive packets |

### UDP Implementation Notes

- **Bidirectional requirement**: Root needs to send TO and receive FROM neighbors. UDP supports this via datagrams
- **Packet loss handling**: Babel protocol (which root implements) is designed for unreliable networks:
  - Periodic route updates compensate for lost packets
  - Sequence numbers detect stale routes
  - Convergence time increases with packet loss (but still works)
- **Implementation**: You'd use `UdpSocket` and handle serialization/deserialization yourself
- **Example**: Similar to TCP example, but use `UdpSocket::send_to()` and `UdpSocket::recv_from()` instead of `TcpStream`

### Unidirectional Protocols

If you have a truly unidirectional protocol (can only send OR receive, not both):
- Model as **two separate links**: one for send direction, one for receive direction
- Each direction gets its own `Link` identifier
- Root will treat them as separate neighbors (which may not be ideal)

## Resources

- [Babel RFC 8966](https://datatracker.ietf.org/doc/html/rfc8966)
- [root GitHub](https://github.com/encodeous/root)
- Examples in `/examples/simple-mesh` show full TCP implementation
- `/examples/super-simple` shows basic concepts

