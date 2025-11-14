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
  ChevronDown,
} from "lucide-react";
import { Button } from "@/components/ui/button";
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from "@/components/ui/select";

// Navigation sections
const navSections = [
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

interface SideNavigationProps {
  activeSection: string;
  onSectionChange: (sectionId: string) => void;
  performanceTheme?: string;
  onPerformanceThemeChange?: (theme: string) => void;
}

export const SideNavigation: React.FC<SideNavigationProps> = ({
  activeSection,
  onSectionChange,
  performanceTheme = "testing",
  onPerformanceThemeChange,
}) => {
  const [isVisible, setIsVisible] = useState(false);
  const [isHoveringNav, setIsHoveringNav] = useState(false);
  const [isDropdownOpen, setIsDropdownOpen] = useState(false);
  const [mouseX, setMouseX] = useState(0);

  useEffect(() => {
    const handleMouseMove = (e: MouseEvent) => {
      setMouseX(e.clientX);

      // Only respond to mouse position if not hovering over nav and dropdown is not open
      if (!isHoveringNav && !isDropdownOpen) {
        const threshold = 20; // Show when mouse is within 20px of left edge

        setIsVisible(e.clientX < threshold);
      }
    };

    const handleMouseLeave = () => {
      // Only hide on mouse leave if not hovering over nav and dropdown is not open
      if (!isHoveringNav && !isDropdownOpen) {
        setIsVisible(false);
      }
    };

    document.addEventListener("mousemove", handleMouseMove);
    document.addEventListener("mouseleave", handleMouseLeave);

    return () => {
      document.removeEventListener("mousemove", handleMouseMove);
      document.removeEventListener("mouseleave", handleMouseLeave);
    };
  }, [isHoveringNav, isDropdownOpen]);

  const handleNavMouseEnter = () => {
    setIsHoveringNav(true);
    setIsVisible(true);
  };

  const handleNavMouseLeave = () => {
    setIsHoveringNav(false);
    // Check if we should hide based on current mouse position and dropdown state
    const threshold = 20;
    if (mouseX >= threshold && !isDropdownOpen) {
      setIsVisible(false);
    }
  };

  // Get contextualized dropdown content based on active section
  const getDropdownContent = () => {
    switch (activeSection) {
      case "performance":
        return {
          label: "Theme",
          value: performanceTheme,
          onChange: onPerformanceThemeChange,
          options: [
            { value: "testing", label: "Testing" },
            { value: "ableset", label: "Ableset" },
          ],
        };
      case "session":
        return {
          label: "Project Type",
          value: "standard",
          onChange: () => {},
          options: [
            { value: "standard", label: "Standard" },
            { value: "template", label: "Template" },
            { value: "live", label: "Live Session" },
          ],
        };
      case "mix":
        return {
          label: "Console",
          value: "standard",
          onChange: () => {},
          options: [
            { value: "standard", label: "Standard" },
            { value: "vintage", label: "Vintage" },
            { value: "modern", label: "Modern" },
          ],
        };
      default:
        return null;
    }
  };

  const dropdownContent = getDropdownContent();

  return (
    <>
      {/* Side Navigation Bar */}
      <div
        className={`fixed top-0 left-0 bottom-0 z-40 transition-transform duration-300 ease-in-out ${
          isVisible ? "translate-x-0" : "-translate-x-full"
        }`}
        onMouseEnter={handleNavMouseEnter}
        onMouseLeave={handleNavMouseLeave}
      >
        {/* Navigation sidebar */}
        <div className="bg-background border-r border-border h-full w-64 shadow-lg">
          <div className="p-4 h-full flex flex-col">
            <div className="mb-6">
              <h2 className="text-lg font-semibold">Navigation</h2>
              <p className="text-sm text-muted-foreground">Select a section</p>
            </div>

            {/* Contextualized Dropdown */}
            {dropdownContent && (
              <div className="mb-6 pb-4 border-b border-border">
                <div className="space-y-2">
                  <label className="text-sm font-medium text-muted-foreground">
                    {dropdownContent.label}
                  </label>
                  <Select
                    value={dropdownContent.value}
                    onValueChange={dropdownContent.onChange}
                    onOpenChange={setIsDropdownOpen}
                  >
                    <SelectTrigger className="w-full">
                      <SelectValue />
                    </SelectTrigger>
                    <SelectContent>
                      {dropdownContent.options.map((option) => (
                        <SelectItem key={option.value} value={option.value}>
                          {option.label}
                        </SelectItem>
                      ))}
                    </SelectContent>
                  </Select>
                </div>
              </div>
            )}

            <div className="flex-1 space-y-2">
              {navSections.map((section) => {
                const Icon = section.icon;
                const isActive = activeSection === section.id;

                return (
                  <Button
                    key={section.id}
                    variant={isActive ? "default" : "ghost"}
                    className={`w-full justify-start h-12 ${
                      isActive ? "bg-primary text-primary-foreground" : ""
                    }`}
                    onClick={() => onSectionChange(section.id)}
                  >
                    <Icon className="h-5 w-5 mr-3" />
                    <span className="font-medium">{section.label}</span>
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
