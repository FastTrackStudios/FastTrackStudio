import React from 'react';
import { createFileRoute } from '@tanstack/react-router';
import { TransportControls } from '@/components/TransportControls';

export const Route = createFileRoute('/transport')({
  component: TransportPage,
});

function TransportPage() {
  return (
    <div className="container mx-auto p-6 max-w-6xl">
      <div className="mb-6">
        <h1 className="text-3xl font-bold tracking-tight">FastTrack Studio Transport</h1>
        <p className="text-muted-foreground mt-2">
          Manage projects and control transport directly via Tauri backend
        </p>
      </div>

      <TransportControls />

      <div className="mt-8 p-4 bg-muted rounded-lg">
        <h3 className="font-semibold mb-2">How to use:</h3>
        <ul className="list-disc list-inside space-y-1 text-sm text-muted-foreground">
          <li>Create or select a project to get started</li>
          <li>Use transport controls to play, pause, stop, and record</li>
          <li>Adjust tempo, position, and time signature in real-time</li>
          <li>Transport state updates automatically while playing</li>
          <li>Each project has its own independent transport state</li>
          <li>All operations are handled directly by the Rust backend</li>
        </ul>
      </div>
    </div>
  );
}
