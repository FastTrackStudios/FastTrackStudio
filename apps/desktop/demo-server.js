#!/usr/bin/env node

const http = require('http');
const { WebSocketServer } = require('ws');
const url = require('url');

// HTTP Server
const server = http.createServer((req, res) => {
  // Enable CORS
  res.setHeader('Access-Control-Allow-Origin', '*');
  res.setHeader('Access-Control-Allow-Methods', 'GET, POST, PUT, DELETE, OPTIONS');
  res.setHeader('Access-Control-Allow-Headers', 'Content-Type, Authorization');

  if (req.method === 'OPTIONS') {
    res.writeHead(200);
    res.end();
    return;
  }

  const parsedUrl = url.parse(req.url, true);
  const pathname = parsedUrl.pathname;

  console.log(`${new Date().toISOString()} - ${req.method} ${pathname}`);

  // Route handlers
  if (pathname === '/api/status') {
    handleStatus(req, res);
  } else if (pathname === '/api/transport') {
    handleTransport(req, res);
  } else if (pathname === '/api/project') {
    handleProject(req, res);
  } else if (pathname === '/api/markers') {
    handleMarkers(req, res);
  } else {
    res.writeHead(404, { 'Content-Type': 'application/json' });
    res.end(JSON.stringify({ error: 'Not Found', path: pathname }));
  }
});

function handleStatus(req, res) {
  const status = {
    server: 'FastTrackStudio Demo Server',
    version: '1.0.0',
    timestamp: new Date().toISOString(),
    uptime: process.uptime(),
    connections: {
      http: true,
      websocket: wss.clients.size
    },
    transport: {
      playing: Math.random() > 0.5,
      position: Math.floor(Math.random() * 300000), // Random position in ms
      bpm: 120 + Math.floor(Math.random() * 60) // Random BPM between 120-180
    }
  };

  res.writeHead(200, { 'Content-Type': 'application/json' });
  res.end(JSON.stringify(status, null, 2));
}

function handleTransport(req, res) {
  if (req.method === 'GET') {
    const transport = {
      playing: Math.random() > 0.5,
      position: Math.floor(Math.random() * 300000),
      bpm: 120,
      timeSignature: { numerator: 4, denominator: 4 },
      loop: {
        enabled: false,
        start: 0,
        end: 0
      }
    };
    res.writeHead(200, { 'Content-Type': 'application/json' });
    res.end(JSON.stringify(transport, null, 2));
  } else if (req.method === 'POST') {
    let body = '';
    req.on('data', chunk => body += chunk);
    req.on('end', () => {
      try {
        const command = JSON.parse(body);
        console.log('Transport command:', command);

        // Broadcast to WebSocket clients
        const response = {
          type: 'transport_update',
          command: command,
          timestamp: new Date().toISOString()
        };

        wss.clients.forEach(client => {
          if (client.readyState === 1) { // WebSocket.OPEN
            client.send(JSON.stringify(response));
          }
        });

        res.writeHead(200, { 'Content-Type': 'application/json' });
        res.end(JSON.stringify({ success: true, command }));
      } catch (error) {
        res.writeHead(400, { 'Content-Type': 'application/json' });
        res.end(JSON.stringify({ error: 'Invalid JSON' }));
      }
    });
  }
}

function handleProject(req, res) {
  const project = {
    name: 'Demo Project',
    sampleRate: 44100,
    bitDepth: 24,
    tracks: [
      { id: 1, name: 'Drums', type: 'audio', armed: false, muted: false, solo: false },
      { id: 2, name: 'Bass', type: 'audio', armed: true, muted: false, solo: false },
      { id: 3, name: 'Piano', type: 'midi', armed: false, muted: false, solo: false },
      { id: 4, name: 'Vocals', type: 'audio', armed: false, muted: true, solo: false }
    ],
    modified: new Date().toISOString()
  };

  res.writeHead(200, { 'Content-Type': 'application/json' });
  res.end(JSON.stringify(project, null, 2));
}

function handleMarkers(req, res) {
  const markers = [
    { id: 1, name: 'Intro', position: 0, type: 'marker' },
    { id: 2, name: 'Verse 1', position: 32000, type: 'marker' },
    { id: 3, name: 'Chorus', position: 64000, type: 'marker' },
    { id: 4, name: 'Bridge', position: 128000, type: 'marker' },
    { id: 5, name: 'Outro', position: 192000, type: 'marker' }
  ];

  res.writeHead(200, { 'Content-Type': 'application/json' });
  res.end(JSON.stringify(markers, null, 2));
}

// WebSocket Server
const wss = new WebSocketServer({ port: 8080 });

wss.on('connection', (ws, req) => {
  console.log(`${new Date().toISOString()} - WebSocket connection established from ${req.socket.remoteAddress}`);

  // Send welcome message
  ws.send(JSON.stringify({
    type: 'welcome',
    message: 'Connected to FastTrackStudio Demo Server',
    timestamp: new Date().toISOString(),
    clientCount: wss.clients.size
  }));

  ws.on('message', (data) => {
    try {
      const message = JSON.parse(data.toString());
      console.log('WebSocket message received:', message);

      // Echo the message back with some processing
      const response = {
        type: 'echo',
        original: message,
        timestamp: new Date().toISOString(),
        processed: true
      };

      // Simulate some DAW-specific responses
      if (message.type === 'transport_command') {
        response.type = 'transport_response';
        response.status = 'success';
        response.command = message.command;
      } else if (message.type === 'track_command') {
        response.type = 'track_response';
        response.trackId = message.trackId;
        response.action = message.action;
        response.result = 'applied';
      }

      ws.send(JSON.stringify(response, null, 2));

      // Broadcast to other clients (except sender)
      wss.clients.forEach(client => {
        if (client !== ws && client.readyState === 1) {
          client.send(JSON.stringify({
            type: 'broadcast',
            from: 'client',
            message: message,
            timestamp: new Date().toISOString()
          }));
        }
      });
    } catch (error) {
      ws.send(JSON.stringify({
        type: 'error',
        message: 'Invalid JSON received',
        timestamp: new Date().toISOString()
      }));
    }
  });

  ws.on('close', () => {
    console.log(`${new Date().toISOString()} - WebSocket connection closed`);
  });

  ws.on('error', (error) => {
    console.error(`${new Date().toISOString()} - WebSocket error:`, error);
  });

  // Send periodic updates
  const interval = setInterval(() => {
    if (ws.readyState === 1) { // WebSocket.OPEN
      ws.send(JSON.stringify({
        type: 'periodic_update',
        transport: {
          position: Math.floor(Math.random() * 300000),
          playing: Math.random() > 0.7
        },
        timestamp: new Date().toISOString()
      }));
    } else {
      clearInterval(interval);
    }
  }, 5000);
});

// Start the servers
const HTTP_PORT = 3000;
const WS_PORT = 8080;

server.listen(HTTP_PORT, () => {
  console.log(`🚀 FastTrackStudio Demo Server running!`);
  console.log(`📡 HTTP Server: http://localhost:${HTTP_PORT}`);
  console.log(`🔌 WebSocket Server: ws://localhost:${WS_PORT}`);
  console.log(`\n📚 Available HTTP endpoints:`);
  console.log(`   GET  /api/status    - Server status and transport info`);
  console.log(`   GET  /api/transport - Transport state`);
  console.log(`   POST /api/transport - Send transport commands`);
  console.log(`   GET  /api/project   - Project information`);
  console.log(`   GET  /api/markers   - Timeline markers`);
  console.log(`\n🔄 WebSocket messages:`);
  console.log(`   - Echoes all messages back with processing`);
  console.log(`   - Sends periodic transport updates every 5s`);
  console.log(`   - Broadcasts messages between connected clients`);
  console.log(`\n💡 Try connecting from the FastTrackStudio Desktop app!`);
});

wss.on('listening', () => {
  console.log(`🔌 WebSocket server listening on port ${WS_PORT}`);
});

// Graceful shutdown
process.on('SIGINT', () => {
  console.log('\n🛑 Shutting down servers...');
  server.close(() => {
    console.log('📡 HTTP server closed');
  });
  wss.close(() => {
    console.log('🔌 WebSocket server closed');
    process.exit(0);
  });
});
