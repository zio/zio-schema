# ZIO Schema brand

Final identity: the Z of ZIO is one strand of a double helix. A violet backbone sweeps from top right to bottom left as the Z's diagonal, and six base pairs hang from it: the top and bottom pairs are the bars, and the four between them, at the same full strength, shorten toward the middle so their ends trace the second strand without drawing it. Each pair joins a light half and a dark half, the way a field joins a name and a type. Space Grotesk wordmark, JetBrains Mono for code.

## Files

**Mark and logo**
- `logo-light.svg`, `logo-dark.svg`: full logo. The mark is the Z of "ZIO", followed by "IO Schema" in Space Grotesk 600 at −3% tracking, with the text converted to outlines.
- `mark-light.svg`, `mark-dark.svg` (and `mark-*-512.png`): the symbol, for 48 px and up.
- `mark-small-light.svg`, `mark-small-dark.svg`: 24 to 47 px. No middle pairs; heavier bars and backbone.
- `mark-tiny-light.svg`, `mark-tiny-dark.svg`: under 24 px. Solid bars and backbone, cut apart by a transparent gap.
- `mark-mono-night.svg`, `mark-mono-white.svg`, `mark-mono-violet.svg`: one-ink versions for print and badges.

**Icons**
- `favicon.svg`: the browser tab icon (16 px drawing on a Night tile). `favicon-dark.svg` adds a 1 px grey rim for dark browser chrome.
- `favicon-32.svg`, `favicon-tile.svg`: the 32 px and 48 px+ drawings.
- `favicon.ico` (16, 32, 48), `favicon-16.png`, `favicon-32.png`, `favicon-48.png`.
- `apple-touch-icon.png` (180), `icon-192.png`, `icon-512.png`: full-bleed square for iOS and the web manifest (`favicon-square.svg`).
- `icon-maskable.svg`, `icon-maskable-512.png`: the mark inside the central 80% safe circle for Android adaptive icons.
- `avatar.svg`, `avatar-500.png`: circle-safe tile for GitHub, Slack and Discord.

**Cards and banners** (PNG at exact platform size, light and dark)
- `github-social-light.png`, `github-social-dark.png`: 1280 × 640, repository Settings › Social preview.
- `og-card-light.png`, `og-card-dark.png`: 1200 × 630, `og:image` (LinkedIn, Slack, Discord, Mastodon).
- `x-card-light.png`, `x-card-dark.png`: 1200 × 675, `twitter:image`.
- `readme-banner-light.png`, `readme-banner-dark.png`: 1200 × 300, switch with `<picture>`.
- `slide-title.png`: 1920 × 1080 talk title slide (placeholders in brackets).
- `sticker-die-cut.svg`: 360 × 360 die-cut sticker with a white border around the mark's shape.

**Tokens and sheet**
- `tokens.css`: every colour, font and radius as `--zs-*` CSS variables, light by default and dark through `prefers-color-scheme`.
- `brand-sheet.html`, `brand-sheet.png`: the one-page overview.
- `sheet/`: the ten brand-sheet sections (logo, icons, colour, type, voice, imagery, hero, social, browser, assets) as canvas boards (`.dc.html`) and PNG.

## Palette

| Role | Light | Dark |
|---|---|---|
| Helix violet: backbone, links, accents | `#6D4AE0` (5.67:1 on white) | `#9C86F2` (6.17:1 on Night) |
| Deep strand: dark half of a pair, keywords, buttons | `#4A2FA6` (9.34:1) | `#8A72F0` (4.99:1) |
| Base light: light half of a pair, fills only | `#B7A6F7` (2.14:1, never text) | `#C9BCFA` (10.47:1) |
| Text / page | Night `#101521` on Paper `#FFFFFF` | Snow `#F4F5F8` on Night `#101521` |
| Secondary text, panel, line | `#4A5568`, `#F1F4F9`, `#D5DBE5` | `#A9B4C6`, `#1A2030`, `#2B3448` |

Violet is ZIO Schema's own hue in the ZIO family: Config is orange-red, Prelude terracotta, Logging amber and HTTP green. Don't use a sibling's hue in Schema material.

## Docs site `<head>`

```html
<link rel="icon" href="/favicon.svg" type="image/svg+xml">
<link rel="icon" href="/favicon-dark.svg" type="image/svg+xml" media="(prefers-color-scheme: dark)">
<link rel="icon" href="/favicon-32.png" sizes="32x32">
<link rel="apple-touch-icon" href="/apple-touch-icon.png">
<meta name="theme-color" content="#6D4AE0">
```

## Rebuild

Needs Python with `fontTools` (run through `uv`), headless Chromium, and the two open-source fonts (`SpaceGrotesk[wght].ttf` and `JetBrainsMono[wght].ttf` from the Google Fonts repository).

```bash
uv run --with fonttools python tools/build_brand.py \
  <SpaceGrotesk[wght].ttf> <JetBrainsMono[wght].ttf> . [<dir with Brand*.dc.html boards>]
```

The script writes the SVG sources and `tokens.css`, rasterises the icons, cards and banners with Chromium, builds `favicon.ico`, and, when given the boards directory, copies the brand-sheet boards to `sheet/` and renders them to PNG.

The tagline "The DNA of your data", the `zio.dev/zio-schema` address and the sample copy are proposals for the maintainers to confirm.
