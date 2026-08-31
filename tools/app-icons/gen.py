#!/usr/bin/env python3
"""Generate the FastTrackStudio iOS app-icon family.

One dark 1024x1024 plate per app, with a single-hue glyph rendered as a
glowing glass tube: a blurred hue pass underneath for the emissive halo,
the glyph itself on a light->deep gradient along the light direction, and
a narrower near-white pass on top for the specular ridge. Same recipe for
every app; only the hue and the glyph change.
"""
import math, os, subprocess, sys

OUT = os.path.dirname(os.path.abspath(__file__))
SIZE = 1024

# hue triplet per app: (highlight, mid, deep)
PALETTE = {
    "signal":   ("#B6F7CB", "#2FD673", "#0E7C42"),   # green
    "session":  ("#B3DBFF", "#2E9BFF", "#0A56C0"),   # blue
    "ignition": ("#FFD3A8", "#FF8A2B", "#BC4407"),   # orange
    "keyflow":  ("#FFBCE4", "#F050B0", "#A31A6A"),   # magenta
    "task":     ("#CAC5FF", "#6C63FF", "#372BC6"),   # indigo (Task's existing hue)
}


def sine_path(x0, x1, ymid, amp, cycles, steps=180, envelope=True):
    """Smooth wave as a polyline. `envelope` tapers both ends to zero so the
    stroke starts and ends flat -- a burst, not a slice out of a sine."""
    pts = []
    for i in range(steps + 1):
        t = i / steps
        x = x0 + (x1 - x0) * t
        env = math.sin(math.pi * t) ** 0.7 if envelope else 1.0
        y = ymid - math.sin(2 * math.pi * cycles * t) * amp * env
        pts.append((x, y))
    return "M " + " L ".join(f"{x:.1f} {y:.1f}" for x, y in pts)


def glyph(app):
    """Return [(kind, attrs)] where kind is 'stroke' or 'fill'.

    Coordinates live in a 1024 box; everything stays inside ~200..824 so the
    glyph clears the corner radius iOS masks on."""
    if app == "signal":
        # An audio burst: 2.5 cycles under a bell envelope.
        return [("stroke", {"d": sine_path(200, 824, 512, 232, 2.5), "stroke-width": 76})]

    if app == "session":
        # Clips on an arrangement -- the set laid out in time. Left-aligned
        # bars read as a hamburger menu; staggering the starts is what makes
        # it a timeline instead.
        bars = [(214, 288, 392), (330, 402, 300), (214, 516, 520), (398, 630, 296)]
        return [("fill", {"d": capsule(x, y, w, 78)}) for x, y, w in bars]

    if app == "ignition":
        # A fixture throwing a beam. The beam IS the glow.
        # The head is deliberately wider than the beam's mouth -- when they
        # matched, the silhouette read as a bottle rather than a fixture.
        return [
            ("fill", {"d": rounded_rect(400, 198, 224, 104, 26)}),
            ("beam", {"d": "M 418 302 L 606 302 L 828 862 L 196 862 Z"}),
        ]

    if app == "keyflow":
        # A staff with the line flowing across it -- chart writing plus the
        # theory under it. Two weights on purpose: the staff is texture that
        # drops back at small sizes, the melodic line carries the read.
        staff = [("stroke", {"d": f"M 214 {y} L 810 {y}", "stroke-width": 26,
                             "opacity": "0.80"})
                 for y in (396, 454, 512, 570, 628)]
        return staff + [
            ("stroke", {"d": sine_path(250, 774, 512, 120, 1.0, envelope=False),
                        "stroke-width": 62}),
        ]

    if app == "task":
        return [("stroke", {"d": "M 296 528 L 452 684 L 734 366", "stroke-width": 94})]

    raise SystemExit(f"unknown app {app}")


def capsule(x, y, w, h):
    return rounded_rect(x, y, w, h, h / 2)


def rounded_rect(x, y, w, h, r):
    return (f"M {x+r} {y} H {x+w-r} A {r} {r} 0 0 1 {x+w} {y+r} "
            f"V {y+h-r} A {r} {r} 0 0 1 {x+w-r} {y+h} H {x+r} "
            f"A {r} {r} 0 0 1 {x} {y+h-r} V {y+r} A {r} {r} 0 0 1 {x+r} {y} Z")


def emit(app):
    hi, mid, deep = PALETTE[app]
    parts = glyph(app)

    def draw(kind, attrs, paint, extra=""):
        a = dict(attrs)
        d = a.pop("d")
        sw = a.pop("stroke-width", None)
        # Anything left over (opacity, say) rides through onto all three
        # passes, so a de-emphasised element stays de-emphasised in its glow
        # and specular too.
        rest = " ".join(f'{k}="{v}"' for k, v in a.items())
        if kind == "stroke":
            return (f'<path d="{d}" fill="none" stroke="{paint}" stroke-width="{sw}" '
                    f'stroke-linecap="round" stroke-linejoin="round" {rest} {extra}/>')
        return f'<path d="{d}" fill="{paint}" {rest} {extra}/>'

    glow, body, spec = [], [], []
    for kind, attrs in parts:
        if kind == "beam":
            # The beam is its own thing: a soft wedge that fades downward,
            # with bright edges. It supplies its own glow, so it is not
            # repeated into the glow/spec passes.
            body.append(f'<path d="{attrs["d"]}" fill="url(#beam)"/>')
            body.append('<path d="M 418 302 L 196 862" stroke="url(#edge)" stroke-width="15" '
                        'stroke-linecap="round" fill="none"/>')
            body.append('<path d="M 606 302 L 828 862" stroke="url(#edge)" stroke-width="15" '
                        'stroke-linecap="round" fill="none"/>')
            body.append(f'<ellipse cx="512" cy="304" rx="92" ry="19" fill="{hi}" '
                        'opacity="0.95" filter="url(#soft)"/>')
            continue
        glow.append(draw(kind, attrs, mid))
        body.append(draw(kind, attrs, "url(#body)"))
        if kind == "stroke":
            a = dict(attrs)
            a["stroke-width"] = attrs["stroke-width"] * 0.40
            spec.append(draw("stroke", a, "url(#spec)",
                             extra=f'transform="translate(0,{-attrs["stroke-width"]*0.17:.1f})"'))
        else:
            spec.append(draw(kind, attrs, "url(#spec)"))

    nl = "\n      "
    return f'''<svg xmlns="http://www.w3.org/2000/svg" width="{SIZE}" height="{SIZE}" viewBox="0 0 {SIZE} {SIZE}">
  <defs>
    <linearGradient id="plate" x1="0" y1="0" x2="0" y2="1">
      <stop offset="0" stop-color="#232327"/>
      <stop offset="0.55" stop-color="#151517"/>
      <stop offset="1" stop-color="#0A0A0C"/>
    </linearGradient>
    <radialGradient id="ambient" cx="0.5" cy="0.5" r="0.62">
      <stop offset="0" stop-color="{mid}" stop-opacity="0.22"/>
      <stop offset="1" stop-color="{mid}" stop-opacity="0"/>
    </radialGradient>
    <!-- userSpaceOnUse, NOT objectBoundingBox: one light direction shared by
         every element, and — the reason it is mandatory — a horizontal line
         has a zero-height bbox, and SVG does not render an
         objectBoundingBox gradient on a zero-extent box at all. That is
         what made Keyflow's staff lines disappear entirely. -->
    <linearGradient id="body" gradientUnits="userSpaceOnUse"
                    x1="180" y1="200" x2="860" y2="880">
      <stop offset="0" stop-color="{hi}"/>
      <stop offset="0.42" stop-color="{mid}"/>
      <stop offset="1" stop-color="{deep}"/>
    </linearGradient>
    <linearGradient id="spec" gradientUnits="userSpaceOnUse"
                    x1="0" y1="250" x2="0" y2="790">
      <stop offset="0" stop-color="#FFFFFF" stop-opacity="0.60"/>
      <stop offset="0.5" stop-color="#FFFFFF" stop-opacity="0.14"/>
      <stop offset="1" stop-color="#FFFFFF" stop-opacity="0"/>
    </linearGradient>
    <linearGradient id="beam" x1="0" y1="0" x2="0" y2="1">
      <stop offset="0" stop-color="{hi}" stop-opacity="0.80"/>
      <stop offset="0.30" stop-color="{mid}" stop-opacity="0.34"/>
      <stop offset="0.78" stop-color="{mid}" stop-opacity="0.03"/>
      <stop offset="1" stop-color="{mid}" stop-opacity="0"/>
    </linearGradient>
    <linearGradient id="edge" x1="0" y1="0" x2="0" y2="1">
      <stop offset="0" stop-color="{hi}" stop-opacity="0.95"/>
      <stop offset="1" stop-color="{mid}" stop-opacity="0"/>
    </linearGradient>
    <filter id="halo" x="-45%" y="-45%" width="190%" height="190%">
      <feGaussianBlur stdDeviation="36"/>
    </filter>
    <filter id="soft" x="-45%" y="-45%" width="190%" height="190%">
      <feGaussianBlur stdDeviation="12"/>
    </filter>
  </defs>

  <rect width="{SIZE}" height="{SIZE}" fill="url(#plate)"/>
  <rect width="{SIZE}" height="{SIZE}" fill="url(#ambient)"/>

  <g filter="url(#halo)" opacity="0.85">
      {nl.join(glow)}
  </g>
  <g>
      {nl.join(body)}
  </g>
  <g>
      {nl.join(spec)}
  </g>
</svg>
'''


# Where each product's iOS app lives, relative to the sibling-repo checkout
# root (the dir holding signal/, session/, ... side by side).
APPS = {
    "signal":   "signal/apps/fasttrackstudio/ios",
    # The session repo's app crate is still named `fasttrackstudio` (a
    # leftover from the monorepo split); only the BUNDLE ID distinguishes
    # the products. Renaming the crate is pending hygiene.
    "session":  "session/apps/fasttrackstudio/ios",
    "ignition": "Ignition/apps/mobile/ios",
    "keyflow":  "keyflow/apps/mobile/ios",
    "task":     "task/apps/mobile/ios",
}

CONTENTS_APPICON = """{
  "images" : [
    { "filename" : "icon-1024.png", "idiom" : "universal", "platform" : "ios", "size" : "1024x1024" }
  ],
  "info" : { "author" : "xcode", "version" : 1 }
}
"""
CONTENTS_ROOT = '{ "info" : { "author" : "xcode", "version" : 1 } }\n'


def rsvg():
    """librsvg, from PATH or from the nix store via imagemagick's delegate."""
    from shutil import which
    exe = which("rsvg-convert")
    if exe:
        return exe
    out = subprocess.run(["magick", "-list", "delegate"], capture_output=True, text=True).stdout
    for line in out.splitlines():
        if "rsvg-convert" in line:
            return line.split("'")[1]
    raise SystemExit("rsvg-convert not found (install librsvg)")


if __name__ == "__main__":
    RSVG = rsvg()
    # Default: the sibling-repo root two levels up from FastTrackStudio/tools/.
    root = sys.argv[1] if len(sys.argv) > 1 else os.path.abspath(
        os.path.join(OUT, "..", "..", ".."))
    for app in PALETTE:
        dest = os.path.join(root, APPS[app])
        icons = os.path.join(dest, "Assets.xcassets", "AppIcon.appiconset")
        os.makedirs(icons, exist_ok=True)
        svg = os.path.join(dest, "icon.svg")
        with open(svg, "w") as f:
            f.write(emit(app))
        png = os.path.join(icons, "icon-1024.png")
        subprocess.run([RSVG, "-w", str(SIZE), "-h", str(SIZE), svg, "-o", png], check=True)
        # Strip alpha. The plate is fully opaque so the channel is all-255,
        # but App Store validation rejects an icon that merely HAS one
        # (ITMS-90717, "Invalid large app icon ... can't be transparent").
        subprocess.run(["magick", png, "-background", "black", "-alpha", "remove",
                        "-alpha", "off", png], check=True)
        with open(os.path.join(icons, "Contents.json"), "w") as f:
            f.write(CONTENTS_APPICON)
        with open(os.path.join(dest, "Assets.xcassets", "Contents.json"), "w") as f:
            f.write(CONTENTS_ROOT)
        print(f"{app:9s} -> {os.path.relpath(dest, root)}")
