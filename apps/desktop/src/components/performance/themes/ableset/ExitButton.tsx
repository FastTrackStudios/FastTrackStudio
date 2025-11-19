import { cn } from "@/lib/utils";
import { X } from "lucide-react";
import * as React from "react";

type ExitButtonProps = {
  className?: string;
  onClick?: () => void;
};

export const ExitButton: React.FC<ExitButtonProps> = ({
  className,
  onClick,
}) => {
  return (
    <button
      className={cn(
        "flex h-12 w-12 items-center justify-center rounded-full transition-colors bg-[var(--ableset-color-background-light)] hover:bg-[var(--ableset-color-hover)]",
        className
      )}
      onClick={onClick}
      title="Exit"
    >
      <X
        className="h-6 w-6 text-[var(--ableset-color-text)]"
        strokeWidth={1.5}
      />
    </button>
  );
};
