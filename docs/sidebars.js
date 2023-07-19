const sidebars = {
  sidebar: [
    {
      type: "category",
      label: "ZIO Schema",
      collapsed: false,
      link: { type: "doc", id: "index" },
      items: [
        "motivation",
        "use-cases",
        "basic-building-blocks",
        {
          type: "category",
          label: "Writing Schema",
          collapsed: true,
          items: [
            "manual-schema-construction",
            "automatic-schema-derivation"
          ],
        },
        "operations",
        "the-default-value",
        "diffing-and-patching",
        "deriving-ordering",
        "schema-migration",
        "schema-serialization",
        "transforming-schemas",
        "validation",
        "codecs",
        "dynamic-data-representation",
        "reified-optics",
        "protobuf-example",
        "combining-different-encoders",
        {
            type: "category",
            label: "Examples",
            collapsed: true,
            items: [
                "mapping-dto-to-domain-object" 
            ],
        }
      ],
    },
  ],
};

module.exports = sidebars;
