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
        "dynamic-data-representation",
        "validation",
        {
          type: "category",
          label: "Codecs",
          collapsed: true,
          link: { type: "doc", id: "codecs/index" },
          items: [
            "codecs/avro",
            "codecs/thrift",
            "codecs/bson",
            "codecs/json",
            "codecs/message-pack",
            "codecs/protobuf"
          ],
        },
        "optics",
        {
            type: "category",
            label: "Examples",
            collapsed: true,
            items: [
                "examples/mapping-dto-to-domain-object",
                "examples/combining-different-encoders",
            ],
        }
      ],
    },
  ],
};

module.exports = sidebars;
