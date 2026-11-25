"""Build the ZIO Schema brand kit: SVG sources, PNG exports, social cards and brand-sheet boards.

Usage (fontTools is the only Python dependency; Chromium does the rasterising):
    uv run --with fonttools python tools/build_brand.py <SpaceGrotesk[wght].ttf> <JetBrainsMono[wght].ttf> <brand dir> [boards dir]

<boards dir> holds the canvas boards (Brand*.dc.html). When given, they are copied to sheet/ and rendered to PNG.
"""
import os
import re
import shutil
import struct
import subprocess
import sys
import tempfile

from fontTools.pens.boundsPen import BoundsPen
from fontTools.pens.svgPathPen import SVGPathPen
from fontTools.pens.transformPen import TransformPen
from fontTools.ttLib import TTFont
from fontTools.varLib.instancer import instantiateVariableFont

SANS, MONO, OUT = (os.path.abspath(p) for p in sys.argv[1:4])
BOARDS = os.path.abspath(sys.argv[4]) if len(sys.argv) > 4 else None
CHROME = shutil.which("chromium") or shutil.which("google-chrome-stable") or shutil.which("chromium-browser")

# ---------------------------------------------------------------- palette
NIGHT, SNOW, PAPER, SLATE, MIST = "#101521", "#F4F5F8", "#FFFFFF", "#4A5568", "#A9B4C6"
PANEL_L, PANEL_D, LINE_L, LINE_D = "#F1F4F9", "#1A2030", "#D5DBE5", "#2B3448"
LIGHT = dict(g1="#6D4AE0", g2="#4A2FA6", dk="#4A2FA6", lt="#B7A6F7", q="1", rib="#6D4AE0")
DARK = dict(g1="#9C86F2", g2="#8A72F0", dk="#8A72F0", lt="#C9BCFA", q="1", rib="#9C86F2")
RIB = "M92 14C92 40 28 80 28 106"
QUIET_DK, QUIET_LT = "M35.3 32H62M44.9 44H54M60 76H75.1M56 88H84.7", "M66 32H84.7M58 44H75.1M44.9 76H56M35.3 88H52"
BAR_LT, BAR_DK = "M29 20H56M68 100H91", "M60 20H91M29 100H64"


# ---------------------------------------------------------------- mark drawings (120-unit grid)
def mark_full(t, uid):
    return (f'<defs><linearGradient id="{uid}" gradientUnits="userSpaceOnUse" x1="92" y1="14" x2="28" y2="106">'
            f'<stop offset="0" stop-color="{t["g1"]}"/><stop offset="1" stop-color="{t["g2"]}"/></linearGradient></defs>'
            f'<g fill="none" stroke-linecap="round"><g stroke-width="8" opacity="{t["q"]}">'
            f'<path d="{QUIET_DK}" stroke="{t["dk"]}"/><path d="{QUIET_LT}" stroke="{t["lt"]}"/></g>'
            f'<g stroke-width="10"><path d="{BAR_LT}" stroke="{t["lt"]}"/><path d="{BAR_DK}" stroke="{t["dk"]}"/></g></g>'
            f'<path d="{RIB}" fill="none" stroke="{NIGHT}" stroke-width="14.5"/>'
            f'<path d="{RIB}" fill="none" stroke="url(#{uid})" stroke-width="10"/>')


def mark_small(t, uid):
    """24 to 47 px: no middle pairs, heavier bars and backbone."""
    return (f'<defs><linearGradient id="{uid}" gradientUnits="userSpaceOnUse" x1="92" y1="14" x2="28" y2="106">'
            f'<stop offset="0" stop-color="{t["g1"]}"/><stop offset="1" stop-color="{t["g2"]}"/></linearGradient></defs>'
            f'<g fill="none" stroke-linecap="round" stroke-width="12"><path d="{BAR_LT}" stroke="{t["lt"]}"/>'
            f'<path d="{BAR_DK}" stroke="{t["dk"]}"/></g>'
            f'<path d="{RIB}" fill="none" stroke="{NIGHT}" stroke-width="17"/>'
            f'<path d="{RIB}" fill="none" stroke="url(#{uid})" stroke-width="12"/>')


def mark_tiny(bar, rib, uid):
    """Under 24 px: solid bars and backbone; a mask cuts a real (transparent) gap around the backbone."""
    return (f'<defs><mask id="{uid}" maskUnits="userSpaceOnUse" x="0" y="0" width="120" height="120">'
            f'<rect width="120" height="120" fill="#fff"/><path d="{RIB}" fill="none" stroke="#000" stroke-width="24"/></mask></defs>'
            f'<path d="M29 20H91M29 100H91" fill="none" stroke="{bar}" stroke-width="16" stroke-linecap="round" mask="url(#{uid})"/>'
            f'<path d="{RIB}" fill="none" stroke="{rib}" stroke-width="17"/>')


def mark_mono(c, uid):
    """One ink: pair halves keep a gap, backbone cut free by a mask."""
    return (f'<defs><mask id="{uid}" maskUnits="userSpaceOnUse" x="0" y="0" width="120" height="120">'
            f'<rect width="120" height="120" fill="#fff"/><path d="{RIB}" fill="none" stroke="#000" stroke-width="14.5"/></mask></defs>'
            f'<g fill="none" stroke="{c}" stroke-linecap="round" mask="url(#{uid})">'
            f'<path d="M35.3 32H60M68 32H84.7M44.9 44H51M61 44H75.1M44.9 76H54M64 76H75.1M35.3 88H50M60 88H84.7" stroke-width="8"/>'
            f'<path d="M29 20H52M64 20H91M29 100H60M72 100H91" stroke-width="10"/></g>'
            f'<path d="{RIB}" fill="none" stroke="{c}" stroke-width="10"/>')


def svg(w, h, vb, body):
    return f'<svg xmlns="http://www.w3.org/2000/svg" width="{w}" height="{h}" viewBox="{vb}">{body}</svg>\n'


def scaled(inner, s):
    return f'<g transform="translate(60 60) scale({s}) translate(-60 -60)">{inner}</g>'


def tile(inner, s, rx=26, rim=False):
    bg = (f'<rect x="4" y="4" width="112" height="112" rx="24" fill="{NIGHT}" stroke="#3C4043" stroke-width="8"/>' if rim
          else f'<rect width="120" height="120" rx="{rx}" fill="{NIGHT}"/>')
    return bg + scaled(inner, s)


# ---------------------------------------------------------------- wordmark outlines
def instance(font_path, w):
    return instantiateVariableFont(TTFont(font_path), {"wght": w})


SG600 = instance(SANS, 600)


def text_outline(font, s, size, x, y, tracking):
    gs, cmap, upm = font.getGlyphSet(), font.getBestCmap(), font["head"].unitsPerEm
    sc = size / upm
    pen, bounds, cx = SVGPathPen(gs), BoundsPen(gs), x
    for ch in s:
        g = cmap[ord(ch)]
        t = (sc, 0, 0, -sc, cx, y)
        gs[g].draw(TransformPen(pen, t))
        gs[g].draw(TransformPen(bounds, t))
        cx += gs[g].width * sc + tracking
    return pen.getCommands(), bounds.bounds, cx


def logo(t, text_fill, uid):
    """The mark is the Z of ZIO at cap height (0.7 em), then 'IO Schema' in Space Grotesk 600, -3% tracking."""
    size, base = 100.0, 100.0
    cap = size * SG600["OS/2"].sCapHeight / SG600["head"].unitsPerEm
    k = cap / 92.0  # tight mark box is 77 x 92 units (x 23..100, y 14..106)
    z_w = 77 * k
    d, (x0, y0, x1, y1), _ = text_outline(SG600, "IO Schema", size, z_w + size * 0.06, base, -0.03 * size)
    top, bottom = min(y0, base - cap) - 4, max(y1, base) + 4
    right = x1 + 4
    body = (f'<g transform="translate(0 {base - cap:.3f}) scale({k:.5f}) translate(-23 -14)">{mark_full(t, uid)}</g>'
            f'<path d="{d}" fill="{text_fill}"/>')
    vb = f"-4 {top:.2f} {right + 4:.2f} {bottom - top:.2f}"
    h = 120
    w = round(h * (right + 4) / (bottom - top))
    return svg(w, h, vb, body)


# ---------------------------------------------------------------- write SVG sources
def write(name, content):
    with open(os.path.join(OUT, name), "w") as f:
        f.write(content)


os.makedirs(OUT, exist_ok=True)
V = "0 0 120 120"
write("mark-light.svg", svg(240, 240, V, mark_full(LIGHT, "g")))
write("mark-dark.svg", svg(240, 240, V, mark_full(DARK, "g")))
write("mark-small-light.svg", svg(32, 32, V, mark_small(LIGHT, "g")))
write("mark-small-dark.svg", svg(32, 32, V, mark_small(DARK, "g")))
write("mark-tiny-light.svg", svg(16, 16, V, mark_tiny("#4A2FA6", "#6D4AE0", "m")))
write("mark-tiny-dark.svg", svg(16, 16, V, mark_tiny("#C9BCFA", "#9C86F2", "m")))
write("mark-mono-night.svg", svg(240, 240, V, mark_mono(NIGHT, "m")))
write("mark-mono-white.svg", svg(240, 240, V, mark_mono("#FFFFFF", "m")))
write("mark-mono-violet.svg", svg(240, 240, V, mark_mono("#6D4AE0", "m")))
write("logo-light.svg", logo(LIGHT, NIGHT, "g"))
write("logo-dark.svg", logo(DARK, SNOW, "g"))
write("favicon.svg", svg(16, 16, V, tile(mark_tiny("#C9BCFA", "#9C86F2", "m"), 0.82)))
write("favicon-dark.svg", svg(16, 16, V, tile(mark_tiny("#C9BCFA", "#9C86F2", "m"), 0.82, rim=True)))
write("favicon-32.svg", svg(32, 32, V, tile(mark_small(DARK, "g"), 0.78)))
write("favicon-tile.svg", svg(48, 48, V, tile(mark_full(DARK, "g"), 0.74)))
write("favicon-square.svg", svg(180, 180, V, tile(mark_full(DARK, "g"), 0.74, rx=0)))
write("icon-maskable.svg", svg(512, 512, V, tile(mark_full(DARK, "g"), 0.62, rx=0)))
write("avatar.svg", svg(500, 500, V, tile(mark_full(DARK, "g"), 0.68, rx=0)))
die = (f'<g fill="none" stroke="#FFFFFF" stroke-linecap="round" stroke-linejoin="round">'
       f'<path d="M29 20H91M29 100H91M35.3 32H84.7M44.9 44H75.1M44.9 76H75.1M35.3 88H84.7" stroke-width="30"/>'
       f'<path d="{RIB}" stroke-width="34"/></g>')
write("sticker-die-cut.svg", svg(360, 360, "-12 -12 144 144", die + mark_full(LIGHT, "g")))

with open(os.path.join(OUT, "tokens.css"), "w") as f:
    f.write("""/* ZIO Schema brand tokens. Light by default, dark through prefers-color-scheme. */
:root {
  --zs-bg: #FFFFFF;
  --zs-surface: #F1F4F9;
  --zs-line: #D5DBE5;
  --zs-text: #101521;
  --zs-text-2: #4A5568;
  --zs-accent: #6D4AE0;      /* Helix violet: backbone, links, accents (5.67:1 on white) */
  --zs-strand: #4A2FA6;      /* Deep strand: dark half of a pair, keywords (9.34:1) */
  --zs-base: #B7A6F7;        /* Base light: light half of a pair, fills only (2.14:1) */
  --zs-link: #6D4AE0;
  --zs-on-accent: #FFFFFF;
  --zs-code-type: #9A5B13;
  --zs-code-string: #0B6E8A;
  --zs-font-sans: "Space Grotesk", system-ui, sans-serif;
  --zs-font-mono: "JetBrains Mono", ui-monospace, monospace;
  --zs-radius-tile: 22%;
}
@media (prefers-color-scheme: dark) {
  :root {
    --zs-bg: #101521;
    --zs-surface: #1A2030;
    --zs-line: #2B3448;
    --zs-text: #F4F5F8;
    --zs-text-2: #A9B4C6;
    --zs-accent: #9C86F2;    /* 6.17:1 on Night */
    --zs-strand: #8A72F0;    /* 4.99:1 */
    --zs-base: #C9BCFA;      /* 10.47:1 */
    --zs-link: #C9BCFA;
    --zs-on-accent: #101521;
    --zs-code-type: #E8B36A;
    --zs-code-string: #7FCBE0;
  }
}
""")

# ---------------------------------------------------------------- HTML renders (cards, banners, slide, boards)
TMP = tempfile.mkdtemp(prefix="zs-brand-")
FONTFACE = (f"@font-face{{font-family:'Space Grotesk';src:url('file://{SANS}') format('truetype');font-weight:300 700}}"
            f"@font-face{{font-family:'JetBrains Mono';src:url('file://{MONO}') format('truetype');font-weight:100 800}}")


def defs_block():
    sym = lambda sid, t, uid: f'<symbol id="{sid}" viewBox="0 0 120 120">{mark_full(t, uid)}</symbol>'
    return ('<svg width="0" height="0" style="position:absolute" aria-hidden="true"><defs>'
            f'<linearGradient id="gL" gradientUnits="userSpaceOnUse" x1="92" y1="14" x2="28" y2="106"><stop offset="0" stop-color="#6D4AE0"/><stop offset="1" stop-color="#4A2FA6"/></linearGradient>'
            f'<linearGradient id="gD" gradientUnits="userSpaceOnUse" x1="92" y1="14" x2="28" y2="106"><stop offset="0" stop-color="#9C86F2"/><stop offset="1" stop-color="#8A72F0"/></linearGradient>'
            f'<path id="rib" d="{RIB}"/>' + sym("mL", LIGHT, "sgL") + sym("mD", DARK, "sgD") +
            '<symbol id="mLt" viewBox="23 14 77 92"><use href="#mL" width="120" height="120"/></symbol>'
            '<symbol id="mDt" viewBox="23 14 77 92"><use href="#mD" width="120" height="120"/></symbol>'
            '</defs></svg>')


def zlogo(size, dark):
    t = "mDt" if dark else "mLt"
    return (f'<div style="font-size:{size}px;line-height:1;letter-spacing:-0.03em;font-weight:600;white-space:nowrap">'
            f'<svg aria-hidden="true" style="height:0.7em;width:0.586em;display:inline-block;vertical-align:baseline;'
            f'margin-right:0.06em;overflow:visible"><use href="#{t}"/></svg>IO Schema</div>')


def pair(dark, h, w="34%"):
    a, b = ("#C9BCFA", "#8A72F0") if dark else ("#B7A6F7", "#4A2FA6")
    return (f'<div style="position:absolute;left:0;bottom:0;height:{h}px;width:{w};display:flex">'
            f'<div style="flex:47;background:{a}"></div><div style="flex:53;background:{b}"></div></div>')


def card(W, H, dark, pad_y=72, pad_x=80, head=88, tile_px=320):
    bg, fg, sub, url = (NIGHT, SNOW, MIST, "#C9BCFA") if dark else (PANEL_L, NIGHT, SLATE, "#4A2FA6")
    tbg, tline = (PANEL_D, LINE_D) if dark else (PAPER, LINE_L)
    grad, op = ("gD", 0.22) if dark else ("gL", 0.16)
    return (f'<div style="width:{W}px;height:{H}px;background:{bg};color:{fg};box-sizing:border-box;padding:{pad_y}px {pad_x}px;'
            f'position:relative;overflow:hidden;display:flex;align-items:center;justify-content:space-between">'
            f'<svg width="780" height="780" viewBox="0 0 120 120" style="position:absolute;right:-230px;top:-160px;opacity:{op}">'
            f'<use href="#rib" fill="none" stroke="url(#{grad})" stroke-width="10"/></svg>'
            f'<div style="display:flex;flex-direction:column;gap:26px;position:relative;max-width:{W - tile_px - pad_x * 2 - 40}px">'
            f'{zlogo(46, dark)}<div style="font-size:{head}px;font-weight:700;letter-spacing:-0.03em;line-height:1">The DNA of your data</div>'
            f'<div style="font-size:29px;color:{sub};line-height:1.4">Describe a type once. Derive codecs, diffs and migrations from it.</div></div>'
            f'<div style="position:relative;width:{tile_px}px;height:{tile_px}px;border-radius:46px;background:{tbg};border:2px solid {tline};'
            f'display:flex;align-items:center;justify-content:center;flex:none"><svg width="{round(tile_px * 0.67)}" height="{round(tile_px * 0.67)}" viewBox="0 0 120 120">'
            f'<use href="#{"mD" if dark else "mL"}"/></svg></div>'
            f'<div style="position:absolute;right:{pad_x}px;bottom:40px;font-family:\'JetBrains Mono\',monospace;font-size:22px;color:{url}">zio.dev/zio-schema</div>'
            f'{pair(dark, 14)}</div>')


def banner(dark):
    bg, fg, sub = (NIGHT, SNOW, MIST) if dark else (PAPER, NIGHT, SLATE)
    grad, op = ("gD", 0.22) if dark else ("gL", 0.16)
    return (f'<div style="width:1200px;height:300px;background:{bg};color:{fg};box-sizing:border-box;padding:0 64px;display:flex;'
            f'align-items:center;position:relative;overflow:hidden"><svg width="520" height="520" viewBox="0 0 120 120" '
            f'style="position:absolute;right:-120px;top:-110px;opacity:{op}"><use href="#rib" fill="none" stroke="url(#{grad})" stroke-width="10"/></svg>'
            f'<div style="display:flex;flex-direction:column;gap:16px;position:relative">{zlogo(104, dark)}'
            f'<div style="font-size:26px;color:{sub}">The DNA of your data. Describe a type once, derive the rest.</div></div>{pair(dark, 10, "30%")}</div>')


def slide():
    return (f'<div style="width:1920px;height:1080px;background:{NIGHT};color:{SNOW};box-sizing:border-box;padding:140px 150px;'
            f'position:relative;overflow:hidden;display:flex;flex-direction:column;justify-content:space-between">'
            f'<svg width="1300" height="1300" viewBox="0 0 120 120" style="position:absolute;right:-380px;top:-260px;opacity:0.22">'
            f'<use href="#rib" fill="none" stroke="url(#gD)" stroke-width="10"/></svg><div style="position:relative">{zlogo(64, True)}</div>'
            f'<div style="display:flex;flex-direction:column;gap:36px;position:relative"><div style="font-size:150px;font-weight:700;'
            f'letter-spacing:-0.03em;line-height:1">[Talk title]</div><div style="font-size:48px;color:{MIST}">[Speaker name] · [Event, date]</div></div>'
            f'{pair(True, 30)}</div>')


def page(body, extra_head=""):
    return (f'<!doctype html><html><head><meta charset="utf-8"><style>{FONTFACE}body{{margin:0;font-family:\'Space Grotesk\',sans-serif}}</style>'
            f'{extra_head}</head><body>{defs_block()}{body}</body></html>')


def shoot(html, w, h, out, transparent=False):
    p = os.path.join(TMP, "p.html")
    with open(p, "w") as f:
        f.write(html)
    args = [CHROME, "--headless", "--no-sandbox", "--disable-gpu", "--hide-scrollbars", "--allow-file-access-from-files",
            f"--window-size={w},{h}", f"--screenshot={out}", "--virtual-time-budget=3000"]
    if transparent:
        args.append("--default-background-color=00000000")
    subprocess.run(args + ["file://" + p], check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)


def shoot_svg(src, size, out):
    shoot(f"<body style='margin:0;background:transparent'><img src='file://{os.path.join(OUT, src)}' width={size} height={size} "
          f"style='display:block'>", size, size, os.path.join(OUT, out), transparent=True)


if not CHROME:
    sys.exit("Chromium not found: SVG sources written, PNGs skipped.")

for src, size, out in [("favicon.svg", 16, "favicon-16.png"), ("favicon-32.svg", 32, "favicon-32.png"),
                       ("favicon-tile.svg", 48, "favicon-48.png"), ("favicon-square.svg", 180, "apple-touch-icon.png"),
                       ("favicon-square.svg", 192, "icon-192.png"), ("favicon-square.svg", 512, "icon-512.png"),
                       ("icon-maskable.svg", 512, "icon-maskable-512.png"), ("avatar.svg", 500, "avatar-500.png"),
                       ("mark-light.svg", 512, "mark-light-512.png"), ("mark-dark.svg", 512, "mark-dark-512.png")]:
    shoot_svg(src, size, out)

pngs = [open(os.path.join(OUT, f"favicon-{n}.png"), "rb").read() for n in (16, 32, 48)]
off, ents, data = 6 + 16 * 3, b"", b""
for n, p in zip((16, 32, 48), pngs):
    ents += struct.pack("<BBBBHHII", n, n, 0, 0, 1, 32, len(p), off + len(data))
    data += p
with open(os.path.join(OUT, "favicon.ico"), "wb") as f:
    f.write(struct.pack("<HHH", 0, 1, 3) + ents + data)

for name, w, h, html in [
    ("og-card-light.png", 1200, 630, card(1200, 630, False)), ("og-card-dark.png", 1200, 630, card(1200, 630, True)),
    ("x-card-light.png", 1200, 675, card(1200, 675, False, pad_y=78)), ("x-card-dark.png", 1200, 675, card(1200, 675, True, pad_y=78)),
    ("github-social-light.png", 1280, 640, card(1280, 640, False, pad_x=84)), ("github-social-dark.png", 1280, 640, card(1280, 640, True, pad_x=84)),
    ("readme-banner-light.png", 1200, 300, banner(False)), ("readme-banner-dark.png", 1200, 300, banner(True)),
    ("slide-title.png", 1920, 1080, slide()),
]:
    shoot(page(html), w, h, os.path.join(OUT, name))

# ---------------------------------------------------------------- brand-sheet boards
if BOARDS:
    order = ["BrandLogo", "BrandIcons", "BrandColour", "BrandType", "BrandVoice", "BrandImagery",
             "BrandHero", "BrandSocial", "BrandBrowser", "BrandAssets"]
    sheet = os.path.join(OUT, "sheet")
    os.makedirs(sheet, exist_ok=True)

    def board_to_html(src, local_fonts=True):
        """Unwrap a canvas board into a plain page. local_fonts: render with the TTFs given on the command line
        (deterministic); otherwise keep the board's Google Fonts link (portable, for the saved overview)."""
        s = open(src).read()
        helmet = re.search(r"<helmet>(.*?)</helmet>", s, re.S).group(1)
        if local_fonts:
            helmet = f"<style>{FONTFACE}</style>" + re.sub(r'<link[^>]+fonts\.google[^>]*>', "", helmet)
        body = re.search(r"<x-dc>(.*?)</x-dc>", s, re.S).group(1)
        body = re.sub(r"<helmet>.*?</helmet>", "", body, flags=re.S)
        w, h = (int(x) for x in re.search(r'"\$preview":\{"width":(\d+),"height":(\d+)\}', s).groups())
        title = re.search(r"<title>(.*?)</title>", s).group(1)
        return (f'<!doctype html><html lang="en"><head><meta charset="utf-8"><title>{title}</title>{helmet}</head>'
                f'<body>{body}</body></html>'), w, h

    for i, name in enumerate(order, 1):
        src = os.path.join(BOARDS, f"{name}.dc.html")
        if not os.path.exists(src):
            continue
        label = f"Brand{i}{name[5:]}"
        shutil.copy(src, os.path.join(sheet, f"{label}.dc.html"))
        html, w, h = board_to_html(src)
        shoot(html, w, h, os.path.join(sheet, f"{label}.png"))
    ov = os.path.join(BOARDS, "BrandOverview.dc.html")
    if os.path.exists(ov):
        html, w, h = board_to_html(ov)
        shoot(html, w, h, os.path.join(OUT, "brand-sheet.png"))
        with open(os.path.join(OUT, "brand-sheet.html"), "w") as f:
            f.write(board_to_html(ov, local_fonts=False)[0])

shutil.rmtree(TMP, ignore_errors=True)
print("done:", OUT)
