# Tarpc Example with OpenTelemetry Tracing

This is a simple tarpc RPC example with OpenTelemetry tracing integration, based on the official tarpc `example-service`.

## 🚀 Quick Start

### Running the Server
```bash
# Start server on default port (50051)
cargo run --bin server

# Or specify a custom port
cargo run --bin server -- --port 8080
```

### Running the Client
```bash
# Connect to default server and send hello with default name
cargo run --bin client

# Or specify custom server and name
cargo run --bin client -- --server-addr "[::1]:8080" --name "Alice"
```

## 📊 OpenTelemetry Tracing

This example includes full OpenTelemetry integration with structured tracing. You'll see traces for:
- RPC call lifecycles
- Request/response timing
- Connection management
- Trace correlation across client/server

### Setting Up Jaeger (Recommended)

The easiest way to view traces is using Jaeger with Docker:

```bash
# Run Jaeger all-in-one
docker run -d --name jaeger \
  -e COLLECTOR_OTLP_ENABLED=true \
  -p 16686:16686 \
  -p 4317:4317 \
  -p 4318:4318 \
  jaegertracing/all-in-one:latest
```

Then:
1. Start your server: `cargo run --bin server`
2. Run some clients: `cargo run --bin client -- --name "Test"`
3. View traces at: http://localhost:16686

## 🔍 Viewing Traces

Once you have Jaeger running:

1. **Start the server:**
   ```bash
   RUST_LOG=info cargo run --bin server
   ```

2. **Make some requests:**
   ```bash
   RUST_LOG=info cargo run --bin client -- --name "Alice"
   RUST_LOG=info cargo run --bin client -- --name "Bob"
   ```

3. **View in Jaeger UI:**
   - Open http://localhost:16686
   - Select service: "Tarpc Example Client" or "Tarpc Example Server"
   - Click "Find Traces"
   - Click on a trace to see the full request flow

## 🐛 Troubleshooting

### "tcp connect error" when running client/server

This error means no OpenTelemetry backend is running. The RPC calls still work fine, but traces can't be exported.

**Solutions:**
1. Start Jaeger: `docker run -d -p 16686:16686 -p 4317:4317 -e COLLECTOR_OTLP_ENABLED=true jaegertracing/all-in-one:latest`
2. Or disable tracing temporarily by setting `OTEL_SDK_DISABLED=true`

### No traces appearing in Jaeger

1. Check Jaeger is running: `docker ps | grep jaeger`
2. Verify OTLP port: `curl http://localhost:4317` (should connect)
3. Check logs for connection errors: `RUST_LOG=debug cargo run --bin client -- --name "Test"`

### Console-only tracing

If you just want console logs without OpenTelemetry:
```bash
# Set environment to disable OTLP export
OTEL_SDK_DISABLED=true RUST_LOG=info cargo run --bin server
```

## 📝 Log Levels

Control logging verbosity with `RUST_LOG`:

```bash
# Minimal logging
RUST_LOG=warn cargo run --bin server

# Default level
RUST_LOG=info cargo run --bin server

# Detailed RPC tracing
RUST_LOG=debug cargo run --bin server

# Everything (very verbose)
RUST_LOG=trace cargo run --bin server
```

## 🏗️ Architecture

- **`src/lib.rs`** - Service definition (`World` trait) and tracing setup
- **`src/server.rs`** - Standalone server binary
- **`src/client.rs`** - Standalone client binary  

The example demonstrates:
- **Service Definition**: Using `#[tarpc::service]` macro
- **Server Implementation**: Async trait implementation with random delays
- **Client Usage**: Generated client with automatic serialization
- **Transport Layer**: JSON over TCP
- **Observability**: Full OpenTelemetry integration
- **Error Handling**: Graceful connection management

## 🔗 Related Links

- [tarpc Documentation](https://docs.rs/tarpc)
- [OpenTelemetry Rust](https://github.com/open-telemetry/opentelemetry-rust)
- [Jaeger Tracing](https://www.jaegertracing.io/)
- [Original Example Service](https://github.com/google/tarpc/tree/master/example-service)