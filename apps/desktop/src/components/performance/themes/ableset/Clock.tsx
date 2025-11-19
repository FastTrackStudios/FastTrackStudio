import React, { useState, useEffect } from "react";
import { cn } from "@/lib/utils";

interface ClockProps {
  className?: string;
  format?: "12h" | "24h";
  showSeconds?: boolean;
}

export const Clock: React.FC<ClockProps> = ({
  className = "",
  format = "12h",
  showSeconds = true,
}) => {
  const [time, setTime] = useState(new Date());

  useEffect(() => {
    const timer = setInterval(() => {
      setTime(new Date());
    }, 1000);

    return () => clearInterval(timer);
  }, []);

  const formatTime = (date: Date): string => {
    if (format === "24h") {
      return date.toLocaleTimeString("en-US", {
        hour12: false,
        hour: "2-digit",
        minute: "2-digit",
        second: showSeconds ? "2-digit" : undefined,
      });
    } else {
      return date.toLocaleTimeString("en-US", {
        hour12: true,
        hour: "numeric",
        minute: "2-digit",
        second: showSeconds ? "2-digit" : undefined,
      });
    }
  };

  return (
    <div
      className={cn(
        "px-4 py-2 rounded-lg font-mono text-lg font-medium",
        "bg-[var(--ableset-color-background-light)]",
        "border border-[var(--ableset-color-border)]",
        "text-[var(--ableset-color-text)]",
        className
      )}
    >
      {formatTime(time)}
    </div>
  );
};
