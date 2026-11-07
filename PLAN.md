# Personal Career Website: Plan

## Goal
A lean, clean, fast personal site that presents my career: who I am, what I've built, and what I think. Minimal design, minimal tooling, easy to update.

## Pages
| Page | Route | Purpose |
|------|-------|---------|
| Home | `/` | Short intro: name, role, one-line pitch, links to other pages, contact/social links |
| About Me | `/about` | Bio, career timeline / experience, skills, resume download, contact |
| Projects | `/projects` | Grid/list of projects: title, short description, tech, links (repo/live) |
| Blogs | `/blog` | List of posts (title, date, summary) + individual post pages |

Shared: a simple top nav (Home · About · Projects · Blog) and a small footer (social links, copyright).

## Recommended Approach: Astro (static site)
- **Why Astro:** ships zero JS by default, so it's extremely fast and lean. Blog posts are just Markdown files. Great defaults for content sites.
- **Alternative (simpler):** plain HTML + CSS, no build step. Fine for 4 pages, but the blog becomes tedious to maintain by hand.
- **Alternative:** Next.js. More powerful but heavier than this site needs.

> Default pick unless you say otherwise: **Astro + plain CSS** (no Tailwind, no UI framework).

## Design Direction
- Minimal, content-first, lots of whitespace
- Single-column layout, max width ~680–720px
- One clean typeface (system font stack or Inter), one accent color
- Light + dark mode via `prefers-color-scheme`
- Fully responsive, mobile-first
- Accessible: semantic HTML, good contrast, keyboard-friendly nav

## Project Structure
```
personal_website/
├── PLAN.md
├── package.json
├── astro.config.mjs
├── public/
│   ├── favicon.svg
│   └── resume.pdf
└── src/
    ├── layouts/
    │   └── BaseLayout.astro      # head, nav, footer
    ├── components/
    │   ├── Nav.astro
    │   ├── Footer.astro
    │   └── ProjectCard.astro
    ├── content/
    │   ├── blog/                 # *.md posts (frontmatter: title, date, summary)
    │   └── projects/             # *.md or a single projects data file
    ├── pages/
    │   ├── index.astro
    │   ├── about.astro
    │   ├── projects.astro
    │   └── blog/
    │       ├── index.astro
    │       └── [...slug].astro
    └── styles/
        └── global.css
```

## Milestones
1. **Scaffold:** init Astro project, base layout, nav, footer, global styles (colors, typography, dark mode)
2. **Pages:** Home, About, Projects, Blog index with placeholder content
3. **Blog:** Markdown content collection, post template, 1–2 sample posts
4. **Polish:** responsive check, accessibility check, meta tags / Open Graph, favicon, RSS feed (optional)
5. **Deploy:** push to GitHub, host on Netlify / Vercel / GitHub Pages / Cloudflare Pages (all free), optional custom domain

## Out of Scope (keeping it lean)
- No CMS, database, or backend
- No comments, analytics, or newsletter at first (easy to add later)
- No animations beyond subtle hover/transition effects

## Open Questions for You
1. **Stack:** OK with Astro + plain CSS, or do you prefer plain HTML/CSS or something else?
2. **Name / title / tagline** to put on the Home page?
3. **Content:** do you have a bio, resume, project list, or any blog posts ready, or should I use placeholders for now?
4. **Style:** any preferred accent color, font, or sites whose look you like?
5. **Hosting / domain:** do you already have a host or domain in mind?
6. **Extras:** want RSS, dark mode toggle (vs. auto), or social links (LinkedIn, GitHub)? Which ones?
