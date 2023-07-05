const sidebars = {
  sidebar: [
    {
      type: "category",
      label: "ZIO Schema",
      collapsed: false,
      link: { type: "doc", id: "index" },
      items: [
        "use-cases",
        {
          type: "category",
          label: "Writing Schema",
          collapsed: true,
          link: { type: "doc", id: "index" },
          items: [
            "manual-schema-construction",
            "automatic-schema-derivation"
          ],
        },
        "motivation",
        "getting-started",
        "transforming-schemas",
        "codecs",
        "protobuf-example",
        "combining-different-encoders",
      ],
    },
  ],
};

module.exports = sidebars;
