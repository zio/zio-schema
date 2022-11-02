// @ts-check
// Note: type annotations allow type checking and IDEs autocompletion

const lightCodeTheme = require('prism-react-renderer/themes/github');
const darkCodeTheme = require('prism-react-renderer/themes/dracula');

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: 'ZIO Schema',
  url: 'https://zio.dev/zio-schema',
  baseUrl: '/',
  onBrokenLinks: 'throw',
  onBrokenMarkdownLinks: 'throw',
  favicon: 'img/favicon.png',

  organizationName: 'zio', 
  projectName: 'zio-schema',

  i18n: {
    defaultLocale: 'en',
    locales: ['en'],
  },
  presets: [
    [
      'classic',
      /** @type {import('@docusaurus/preset-classic').Options} */
      {
        docs: {
          id: 'default',
          path: '../zio-schema-docs/target/mdoc/',
          routeBasePath: '/',
          sidebarPath: require.resolve('../zio-schema-docs/target/mdoc/sidebar.js'),
        },
        theme: {
          customCss: require.resolve('./src/css/custom.css'),
        },
        blog: false,
      },
    ],
  ],
  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    ({
      navbar: {
        title: 'ZIO Schema',
      },
      prism: {
        theme: lightCodeTheme,
        darkTheme: darkCodeTheme,
      },
    }),
};

module.exports = config;
