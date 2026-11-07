// Central place for site-wide info. Edit here, updates everywhere.
export const site = {
  name: 'Gaby Bui',
  role: 'Senior Data Associate | BI Developer',
  tagline: 'I turn messy, multi-platform data into numbers people can trust.',
  description: 'Personal website of Gaby Bui, Senior Data Associate and BI Developer based in Auckland, New Zealand.',
  email: 'gabybui93@gmail.com',
  location: 'Auckland, New Zealand',
};

// Replace the placeholder URLs with your real profiles.
export const socials = [
  { label: 'GitHub', href: 'https://github.com/Gabybui' },
  { label: 'LinkedIn', href: 'https://www.linkedin.com/in/gaby-bui' },
  { label: 'Email', href: `mailto:${site.email}` },
];

export const projects = [
  {
    title: 'Project One',
    description: 'Placeholder: a short description of what this project does and the impact it had.',
    tags: ['Python', 'SQL'],
    href: '#',
  },
  {
    title: 'Project Two',
    description: 'Placeholder: a short description of what this project does and the impact it had.',
    tags: ['Dashboard', 'Analytics'],
    href: '#',
  },
  {
    title: 'Project Three',
    description: 'Placeholder: a short description of what this project does and the impact it had.',
    tags: ['Machine Learning'],
    href: '#',
  },
];

export const experience = [
  {
    role: 'Senior Data Associate | BI Developer',
    company: 'Monks',
    location: 'Auckland, New Zealand',
    period: 'Apr 2025 – Present (Data Associate, Apr 2024 – Apr 2025)',
    bullets: [
      'Designed and implemented web tracking architecture (GA4, GTM client and server-side, Consent Mode v2), improving data control and reducing browser-side tracking loss.',
      'Built and optimised BigQuery and Dataform ELT pipelines with a layered (staging, dimensions, fact marts) design, cutting downstream query costs and stabilising production runs.',
      'Built Data Studio dashboards for multiple clients, covering marketing performance, attribution and cost monitoring.',
      'Investigated and resolved data-integrity issues: tracking incidents, bot traffic and PII compliance risk.',
      'Partnered with clients, engineers, analysts and media teams to turn business needs into tracking and reporting solutions, and authored internal playbooks for server-side GTM migration and audience testing.',
    ],
    tools: 'BigQuery, BigQuery ML, Dataform, GCP, Looker Studio, GA4, GTM, CM360, SA360, Meta CAPI, SQL, Python, Claude Code',
  },
  {
    role: 'Data Science Intern',
    company: 'NIWA',
    location: 'Christchurch, New Zealand',
    period: 'Sep 2023 – Dec 2023',
    bullets: [
      "Modelled flood inundation scenarios in Hawke's Bay and Tairāwhiti for Cyclone Gabrielle recovery using NIWA's BG-Flood software.",
      'Simulated future extreme-event and stop-bank breach scenarios, with and without climate change.',
      'Carried out geospatial analysis and scenario modelling on high-resolution spatial data.',
    ],
  },
  {
    role: 'Sales Manager',
    company: 'AgroChemical Fertilizer Company',
    location: 'Ho Chi Minh City, Vietnam',
    period: 'Feb 2016 – Feb 2020',
    bullets: [],
  },
  {
    role: 'Project Executive',
    company: 'Top3 Media',
    location: 'Ho Chi Minh City, Vietnam',
    period: 'May 2015 – Feb 2016',
    bullets: ['Managed web and app design and development projects (WordPress, Magento) and social media for clients.'],
  },
];

export const skills = [
  { area: 'Data engineering', items: 'GCP, BigQuery SQL, Dataform (incremental models, SQLX), ELT pipeline design, dimensional modelling, cost optimisation, Snowflake' },
  { area: 'Web analytics & martech', items: 'GA4 and GTM (client and server-side), Consent Mode v2, CM360, SA360, Google Ads, Meta, Reddit, LinkedIn, Amazon DSP' },
  { area: 'Programming', items: 'SQL, Python (Pandas, scikit-learn, PyTorch), R, PySpark, JavaScript' },
  { area: 'BI & visualisation', items: 'Streamlit, Data Studio, Power BI' },
  { area: 'Geospatial', items: 'ArcGIS, QGIS, spatial analysis' },
  { area: 'Other', items: 'Git, Bash, LaTeX' },
];

export const education = [
  {
    degree: 'Master of Applied Data Science, with Distinction',
    school: 'University of Canterbury, Christchurch, NZ',
    period: 'Jul 2022 – Feb 2024 · GPA 8.33/9.0',
    notes: 'Capstone: DATA601 Applied Data Science Project (grade A). Coursework: Statistics, Data Mining, Big Data, Scalable Data Science, Spatial Data Science, Data Science in Industry.',
  },
  {
    degree: 'BSc in Business Administration',
    school: 'University of Economics Ho Chi Minh City, Vietnam',
    period: 'Sep 2011 – May 2015',
    notes: '',
  },
];

export const certifications =
  'University of California, Davis (2023): Fundamentals of GIS · GIS Data Formats, Design and Quality · Geospatial and Environmental Analysis · Imagery, Automation, and Applications';

export const awards = [
  'Master of Applied Data Science, with Distinction, University of Canterbury (2024)',
  'Top 5 Performer, MBIS623 SQLoquence Hall of Fame, University of Canterbury',
];

export const languages = 'Vietnamese (native) · English (professional working) · German (elementary)';
