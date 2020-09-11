const apiUrl = "/Proptics/api/proptics/index.html";

const siteConfig = {
  title: 'Proptics[_, _]',
  tagline: 'Profunctor optics and lenses library for Scala',
  url: 'https://sagifogel.github.io/proptics',
  baseUrl: '/Proptics/',
  apiUrl,
  repoUrl: 'https://github.com/sagifogel/proptics',
  customDocsPath: "docs/target/mdoc",
  projectName: 'proptics',
  organizationName: 'com.github.sagifogel',
  headerLinks: [
    {href: apiUrl, label: "API Docs"},
    {href: 'https://github.com/sagifogel/proptics', label: 'GitHub'}
  ],
  colors: {
    primaryColor: '#B9459A',
    secondaryColor: '#8A3373',
  },
  copyright: `Copyright © ${new Date().getFullYear()} Sagi Fogel (foldl)`,
  usePrism: ['scala'],
  highlight: {
    theme: 'atom-one-dark',
  },
  separateCss: ["api"],
  scripts: ['https://buttons.github.io/buttons.js'],
  onPageNav: 'separate',
  cleanUrl: true
};

module.exports = siteConfig;
