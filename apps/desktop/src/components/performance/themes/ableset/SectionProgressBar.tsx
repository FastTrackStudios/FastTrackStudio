import React from "react";

interface SectionProgressBarProps {
  className?: string;
}

export const SectionProgressBar: React.FC<SectionProgressBarProps> = ({
  className = "",
}) => {
  // For now, just show a test bar that's always visible
  const testProgress = 45; // 45% for testing

  return (
    <div className={`w-full ${className}`}>
      {/* Section Progress Bar */}
      <div
        className="h-2 rounded-xl overflow-hidden relative"
        style={{
          backgroundColor: "var(--ableset-color-background-light)",
        }}
      >
        {/* Progress fill */}
        <div
          className="h-full transition-all duration-200 ease-out"
          style={{
            width: `${testProgress}%`,
            backgroundColor: "var(--ableset-color-default-500)",
          }}
        />
      </div>
    </div>
  );
};
