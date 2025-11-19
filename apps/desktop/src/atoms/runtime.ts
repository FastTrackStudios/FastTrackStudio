import { Atom } from "@effect-atom/atom-react";
import { FetchHttpClient } from "@effect/platform";
import * as Socket from "@effect/platform/Socket";
import * as Layer from "effect/Layer";

// Create the runtime for Effect Atom with WebSocket support
export const runtime = Atom.runtime(
  FetchHttpClient.layer.pipe(
    Layer.provide(Socket.layerWebSocketConstructorGlobal)
  )
);

