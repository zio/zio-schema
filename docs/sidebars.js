const sidebars = {
  sidebar: [
    {
      type: "category",
      label: "ZIO Schema",
      collapsed: false,
      link: { type: "doc", id: "index" },
      items: [ 
        "use-cases",
        "our-first-schema",
        "motivation",
        "getting-started",
        "transforming-schemas",
        "codecs",
        "protobuf-example",
        "combining-different-encoders"
      ]
    }
  ]
};

module.exports = sidebars;
