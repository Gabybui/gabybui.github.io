// Prefix internal links with the site's base path (needed for project-repo hosting on GitHub Pages).
const base = import.meta.env.BASE_URL.replace(/\/$/, '');
export const url = (path: string = '/') => `${base}${path.startsWith('/') ? path : `/${path}`}`;
// Path of the current page without the base prefix.
export const stripBase = (pathname: string) => (base && pathname.startsWith(base) ? pathname.slice(base.length) : pathname) || '/';
