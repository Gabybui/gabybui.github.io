import { defineConfig } from 'astro/config';

// GitHub Pages: if the repo is named <username>.github.io, no `base` is needed.
// For any other repo name, add: base: '/<repo-name>'
export default defineConfig({
  site: 'https://gabybui.github.io',
});
