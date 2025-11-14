import React, { useState, useEffect } from "react";
import {
  FolderOpen,
  PenTool,
  Mic,
  Edit3,
  Sliders,
  Volume2,
  Play,
  Download,
  Stethoscope,
  ChevronUp,
} from "lucide-react";
import { Button } from "@/components/ui/button";

// Bottom navigation sections
const bottomNavSections = [
  { id: "session", label: "Session Manager", icon: FolderOpen },
  { id: "writing", label: "Writing", icon: PenTool },
  { id: "recording", label: "Recording", icon: Mic },
  { id: "editing", label: "Editing", icon: Edit3 },
  { id: "mix", label: "Mix", icon: Sliders },
  { id: "master", label: "Master", icon: Volume2 },
  { id: "performance", label: "Performance", icon: Play },
  { id: "export", label: "Export", icon: Download },
  { id: "diagnostics", label: "Diagnostics", icon: Stethoscope },
];

interface BottomNavigationProps {
  activeSection: string;
  onSectionChange: (sectionId: string) => void;
}

export const BottomNavigation: React.FC<BottomNavigationProps> = ({
  activeSection,
  onSectionChange,
}) => {
  const [isVisible, setIsVisible] = useState(false);
  const [isPinned, setIsPinned] = useState(false);
  const [isHoveringNav, setIsHoveringNav] = useState(false);
  const [mouseY, setMouseY] = useState(0);

  useEffect(() => {
    const handleMouseMove = (e: MouseEvent) => {
      setMouseY(e.clientY);

      // Only respond to mouse position if not pinned and not hovering over nav
      if (!isPinned && !isHoveringNav) {
        const windowHeight = window.innerHeight;
        const threshold = windowHeight - 20; // Show when mouse is within 20px of bottom

        setIsVisible(e.clientY > threshold);
      }
    };

    const handleMouseLeave = () => {
      // Only hide on mouse leave if not pinned and not hovering over nav
      if (!isPinned && !isHoveringNav) {
        setIsVisible(false);
      }
    };

    document.addEventListener("mousemove", handleMouseMove);
    document.addEventListener("mouseleave", handleMouseLeave);

    return () => {
      document.removeEventListener("mousemove", handleMouseMove);
      document.removeEventListener("mouseleave", handleMouseLeave);
    };
  }, [isPinned, isHoveringNav]);

  // Update visibility when pinned state changes
  useEffect(() => {
    if (isPinned) {
      setIsVisible(true);
    }
  }, [isPinned]);

  const handleTabClick = () => {
    setIsPinned(!isPinned);
    if (!isPinned) {
      // When pinning, make sure it's visible
      setIsVisible(true);
    }
  };

  const handleNavMouseEnter = () => {
    setIsHoveringNav(true);
    setIsVisible(true);
  };

  const handleNavMouseLeave = () => {
    setIsHoveringNav(false);
    // If not pinned, check if we should hide based on current mouse position
    if (!isPinned) {
      const windowHeight = window.innerHeight;
      const threshold = windowHeight - 20;
      if (mouseY <= threshold) {
        setIsVisible(false);
      }
    }
  };

  return (
    <>
      {/* Bottom Navigation Bar with connected tab */}
      <div
        className={`fixed bottom-0 left-0 right-0 z-40 transition-transform duration-300 ease-in-out ${
          isVisible ? "translate-y-0" : "translate-y-full"
        }`}
        onMouseEnter={handleNavMouseEnter}
        onMouseLeave={handleNavMouseLeave}
      >
        {/* Connected tab indicator */}
        <div className="absolute -top-8 left-4">
          <button
            onClick={handleTabClick}
            className={`bg-background border border-border border-b-0 rounded-t-lg px-3 py-1 shadow-lg transition-colors hover:bg-muted ${
              isPinned ? "bg-primary/10 border-primary" : ""
            }`}
          >
            <div className="flex items-center gap-2">
              <ChevronUp
                className={`h-3 w-3 transition-colors ${
                  isPinned ? "text-primary" : "text-muted-foreground"
                }`}
              />
              <span
                className={`text-xs transition-colors ${
                  isPinned
                    ? "text-primary font-medium"
                    : "text-muted-foreground"
                }`}
              >
                Navigation {isPinned ? "(Pinned)" : ""}
              </span>
            </div>
          </button>
        </div>

        {/* Navigation bar */}
        <div className="bg-background border-t border-border">
          <div className="container mx-auto px-4">
            <div className="flex items-center justify-between py-2">
              {bottomNavSections.map((section) => {
                const Icon = section.icon;
                const isActive = activeSection === section.id;

                return (
                  <Button
                    key={section.id}
                    variant={isActive ? "default" : "ghost"}
                    size="sm"
                    className={`flex-1 flex flex-col items-center gap-1 h-auto py-3 px-2 mx-1 min-h-[60px] ${
                      isActive ? "bg-primary text-primary-foreground" : ""
                    }`}
                    onClick={() => onSectionChange(section.id)}
                  >
                    <Icon className="h-4 w-4" />
                    <span className="text-xs font-medium text-center leading-tight">
                      {section.label}
                    </span>
                  </Button>
                );
              })}
            </div>
          </div>
        </div>
      </div>
    </>
  );
};
