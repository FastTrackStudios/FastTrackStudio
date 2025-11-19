import { Atom, Registry } from "@effect-atom/atom-react";
import { Effect, Console, Schedule, Duration } from "effect";
import type { SetlistMessage, Setlist, TransportStates } from "./types";

// Connection state type
export type ConnectionState = 'connecting' | 'connected' | 'disconnected' | 'error';

// Connection state atom
export const connectionStateAtom = Atom.make<ConnectionState>('disconnected');

// Setlist atom
export const setlistAtom = Atom.make<Setlist | null>(null);

// Active song index atom
export const activeSongIndexAtom = Atom.make<number | null>(null);

// Transport states atom
export const transportStatesAtom = Atom.make<TransportStates>(new Map());

// Module-level mutable ref for WebSocket instance (for synchronous access from action atoms)
const wsInstanceRef: { current: WebSocket | null } = { current: null };

// WebSocket URL helper
const getWebSocketUrl = (): string => {
  const wsProtocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
  return `${wsProtocol}//${window.location.hostname}:8080/ws`;
};

// Process a WebSocket message and update atoms
const processMessage = (
  message: SetlistMessage,
  getContext: Atom.FnContext,
  transportStatesRef: { current: TransportStates },
): void => {
  switch (message.type) {
    case 'SetlistUpdate':
      console.log('SetlistUpdate received:', message.setlist);
      // Debug: Log first song's region marker colors
      if (message.setlist?.songs?.[0]) {
        const firstSong = message.setlist.songs[0];
        console.log('[WebSocket] First song region markers:', {
          name: firstSong.name,
          song_region_start_marker: firstSong.song_region_start_marker,
          song_region_end_marker: firstSong.song_region_end_marker,
          start_marker: firstSong.start_marker,
        });
      }
      getContext.set(setlistAtom, message.setlist);
      if (message.active_song_index !== undefined && message.active_song_index !== null) {
        getContext.set(activeSongIndexAtom, message.active_song_index);
      }
      break;
      
    case 'TransportUpdate':
      // Update transport states atom
      const currentTransportStates = transportStatesRef.current;
      const next = new Map(currentTransportStates);
      const oldState = currentTransportStates.get(message.project_name);
      
      // Track when a project starts playing
      let started_playing_at = oldState?.started_playing_at;
      if (message.playing && !oldState?.playing) {
        started_playing_at = Date.now();
      } else if (!message.playing) {
        started_playing_at = undefined;
      }
      
      next.set(message.project_name, {
        is_active: message.is_active,
        playing: message.playing,
        position: message.position,
        tempo: message.tempo,
        song_progress: message.song_progress,
        section_progress: message.section_progress,
        started_playing_at,
      });
      getContext.set(transportStatesAtom, next);
      // Update ref for next message
      transportStatesRef.current = next;
      break;
      
    case 'Ping':
      // Respond to ping with pong
      const ws = wsInstanceRef.current;
      if (ws && ws.readyState === WebSocket.OPEN) {
        try {
          ws.send(JSON.stringify({ type: 'Pong' }));
        } catch (error) {
          console.error('Failed to send pong:', error);
        }
      }
      break;
      
    case 'Pong':
      // Pong received, connection is alive
      break;
  }
};

// Create WebSocket connection using raw WebSocket API (more reliable than Effect Socket API)
const createWebSocketConnection = (
  url: string,
  getContext: Atom.FnContext,
): Effect.Effect<WebSocket, never, never> =>
  Effect.async<WebSocket>((resume) => {
    getContext.set(connectionStateAtom, 'connecting');
    console.log('Creating WebSocket connection to:', url);
    
    const ws = new WebSocket(url);
    
    const onOpen = () => {
      console.log('✅ WebSocket opened');
      getContext.set(connectionStateAtom, 'connected');
      resume(Effect.succeed(ws));
    };
    
    const onError = (error: Event) => {
      console.error('❌ WebSocket error:', error);
      getContext.set(connectionStateAtom, 'error');
      resume(Effect.die(error));
    };
    
    ws.onopen = onOpen;
    ws.onerror = onError;
    
    // Return cleanup function
    return Effect.sync(() => {
      ws.onopen = null;
      ws.onerror = null;
    });
  });

// WebSocket connection atom using Effect's Socket API
// This atom auto-starts when accessed and stays alive using keepAlive
export const wsConnectionAtom = Atom.make(
  Effect.fn(function* (get) {
    // Capture get context for use in WebSocket callbacks
    const getContext = get;
    
    // Start WebSocket connection in background fiber with reconnection
    yield* Effect.forkScoped(
      Effect.gen(function* () {
        const url = getWebSocketUrl();
        
        // Transport states ref for message processing
        const transportStatesRef = { current: new Map() as TransportStates };
        
        // Use Schedule for reconnection with exponential backoff
        yield* Effect.repeat(
          Effect.gen(function* () {
            getContext.set(connectionStateAtom, 'connecting');
            console.log('Attempting to connect to WebSocket:', url);
            
            // Create WebSocket connection using raw WebSocket API
            const ws = yield* createWebSocketConnection(url, getContext).pipe(
              Effect.catchAll((error) => {
                console.error('Failed to create WebSocket:', error);
                getContext.set(connectionStateAtom, 'error');
                // Return a failure so Effect.repeat can retry
                return Effect.fail(error);
              })
            );
            
            console.log('WebSocket created, setting up message handlers...');
            
            // Store WebSocket instance for sending messages
            wsInstanceRef.current = ws;
            
            // Set up message handler
            const onMessage = (event: MessageEvent) => {
              console.log('📨 Message received!', event.data.length, 'bytes');
              try {
                const text = typeof event.data === 'string' ? event.data : new TextDecoder().decode(event.data);
                console.log('📨 Decoded message text:', text.substring(0, 200));
                const message: SetlistMessage = JSON.parse(text);
                console.log('📨 Parsed message type:', message.type);
                processMessage(message, getContext, transportStatesRef);
              } catch (error) {
                console.error('❌ Failed to parse WebSocket message:', error);
              }
            };
            
            ws.onmessage = onMessage;
            
            // Request setlist immediately on connection
            console.log('Sending RequestSetlist message...');
            try {
              ws.send(JSON.stringify({ type: 'RequestSetlist' }));
              console.log('✅ Requested setlist update from server');
            } catch (error) {
              console.error('Failed to send setlist request:', error);
            }
            
            // Wait for WebSocket to close
            yield* Effect.async<void>((resume) => {
              const onClose = () => {
                console.log('WebSocket disconnected');
                getContext.set(connectionStateAtom, 'disconnected');
                wsInstanceRef.current = null;
                resume(Effect.void);
              };
              
              ws.onclose = onClose;
              
              // Return cleanup function
              return Effect.sync(() => {
                ws.onclose = null;
                ws.onmessage = null;
              });
            });
              
            // Connection closed - cleanup already handled in onClose handler
            console.log('WebSocket connection closed');
          }),
          Schedule.exponential(Duration.seconds(2)).pipe(
            Schedule.compose(Schedule.recurs(Infinity)), // Retry indefinitely with exponential backoff
            Schedule.union(Schedule.spaced(Duration.seconds(30))), // Or wait max 30 seconds between attempts
            Schedule.intersect(Schedule.spaced(Duration.seconds(2))), // Minimum 2 seconds between attempts
          ),
        ).pipe(
          Effect.catchAll((error) => {
            console.error('Connection loop failed:', error);
            getContext.set(connectionStateAtom, 'error');
            // Wait a bit before retrying
            return Effect.sleep(Duration.seconds(2)).pipe(
              Effect.flatMap(() => Effect.fail(error))
            );
          })
        );
      }),
    );
    
    return undefined;
  }),
).pipe(
  // Keep the connection alive so it doesn't reset when components unmount
  Atom.keepAlive,
);

// Switch to project action atom
export const switchToProjectAtom = Atom.fn(
  Effect.fn(function* (projectName: string) {
    // Use module-level ref for WebSocket instance
    const ws = wsInstanceRef.current;
    
    if (ws && ws.readyState === WebSocket.OPEN) {
      const message: SetlistMessage = { type: 'SwitchToProject', project_name: projectName };
      try {
        ws.send(JSON.stringify(message));
        yield* Console.log(`Requested switch to project: ${projectName}`);
      } catch (error) {
        yield* Console.warn(`Failed to send switch project message: ${error}`);
      }
    } else {
      // Get state for error message only
      const registry = yield* Registry.AtomRegistry;
      const connectionState = registry.get(connectionStateAtom);
      yield* Console.warn(`WebSocket not connected (state: ${connectionState}), cannot switch project`);
    }
  }),
);

// Seek to section action atom
export const seekToSectionAtom = Atom.fn(
  Effect.fn(function* (input: { projectName: string; songName: string; sectionName: string }) {
    // Use module-level ref for WebSocket instance
    const ws = wsInstanceRef.current;
    
    if (ws && ws.readyState === WebSocket.OPEN) {
      const message: SetlistMessage = {
        type: 'SeekToSection',
        project_name: input.projectName,
        song_name: input.songName,
        section_name: input.sectionName,
      };
      try {
        ws.send(JSON.stringify(message));
        yield* Console.log(`Requested seek to section: ${input.projectName}/${input.songName}/${input.sectionName}`);
      } catch (error) {
        yield* Console.warn(`Failed to send seek section message: ${error}`);
      }
    } else {
      // Get state for error message only
      const registry = yield* Registry.AtomRegistry;
      const connectionState = registry.get(connectionStateAtom);
      yield* Console.warn(`WebSocket not connected (state: ${connectionState}), cannot seek to section`);
    }
  }),
);
