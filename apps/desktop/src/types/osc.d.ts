declare module 'osc' {
  export interface OSCMessage {
    address: string;
    args: any[];
  }

  export interface UDPPortOptions {
    localAddress?: string;
    localPort?: number;
    remoteAddress?: string;
    remotePort?: number;
    metadata?: boolean;
  }

  export class UDPPort {
    constructor(options: UDPPortOptions);

    on(event: 'ready', listener: () => void): void;
    on(event: 'message', listener: (oscMsg: OSCMessage) => void): void;
    on(event: 'error', listener: (error: Error) => void): void;
    on(event: 'close', listener: () => void): void;

    open(): void;
    close(): void;
    send(msg: OSCMessage, address?: string, port?: number): void;
  }

  export interface SerialPortOptions {
    devicePath: string;
    bitrate?: number;
  }

  export class SerialPort {
    constructor(options: SerialPortOptions);

    on(event: 'ready', listener: () => void): void;
    on(event: 'message', listener: (oscMsg: OSCMessage) => void): void;
    on(event: 'error', listener: (error: Error) => void): void;

    open(): void;
    close(): void;
    send(msg: OSCMessage): void;
  }

  export interface WebSocketPortOptions {
    url: string;
    metadata?: boolean;
  }

  export class WebSocketPort {
    constructor(options: WebSocketPortOptions);

    on(event: 'ready', listener: () => void): void;
    on(event: 'message', listener: (oscMsg: OSCMessage) => void): void;
    on(event: 'error', listener: (error: Error) => void): void;

    open(): void;
    close(): void;
    send(msg: OSCMessage): void;
  }

  export function readPacket(data: Buffer, options?: any): OSCMessage | OSCMessage[];
  export function writePacket(msg: OSCMessage | OSCMessage[], options?: any): Buffer;
}
