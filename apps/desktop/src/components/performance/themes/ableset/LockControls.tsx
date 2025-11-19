import { cn } from "@/lib/utils";
import { Lock, Unlock } from "lucide-react";
import * as React from "react";

type LockControlsProps = {
  className?: string;
};

export const LockControls: React.FC<LockControlsProps> = ({ className }) => {
  const [isLocked, setIsLocked] = React.useState(false);

  return (
    <button
      onClick={() => {
        setIsLocked(!isLocked);
      }}
      className={cn(
        "flex h-12 w-12 items-center justify-center rounded-full",
        isLocked
          ? "bg-[var(--ableset-color-destructive)] text-[var(--ableset-color-text)]"
          : "bg-[var(--ableset-color-background-light)] text-[var(--ableset-color-text)] transition-colors hover:bg-[var(--ableset-color-hover)]",
        className
      )}
      title={isLocked ? "Unlock controls" : "Lock controls"}
    >
      {isLocked ? <Lock className="h-6 w-6" /> : <Unlock className="h-6 w-6" />}
    </button>
  );
};
