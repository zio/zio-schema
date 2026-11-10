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
        {
          type: "category",
          label: "Getting Started",
          collapsed: false,
          items: ["tutorial/first-schema"],
        },
        "basic-building-blocks",
        {
          type: "category",
          label: "Reference",
          collapsed: false,
          items: [
            "standard-type-reference",
            "reference/schema-methods",
            "reference/patch-and-differ",
            "reference/name-format-and-type-id",
            "reference/codec-core",
          ],
        },
        {
          type: "category",
          label: "Writing Schema",
          collapsed: true,
          items: ["manual-schema-construction", "automatic-schema-derivation"],
        },
        {
          type: "category",
          label: "Operations",
          link: { type: "doc", id: "operations/index" },
          collapsed: true,
          items: [
            "operations/the-default-value",
            "operations/transforming-schemas",
            "operations/validation",
            "operations/diffing-and-patching",
            "operations/schema-migration",
            "operations/schema-serialization",
            "operations/dynamic-data-representation",
          ],
        },
        {
          type: "category",
          label: "Derivations",
          collapsed: true,
          items: [
            "derivations/derivation-scala-versions",
            "derivations/ordering-derivation",
            "derivations/optics-derivation",
            "derivations/zio-test-gen-derivation",
            {
              type: "category",
              label: "Codecs",
              collapsed: true,
              link: { type: "doc", id: "derivations/codecs/index" },
              items: [
                "derivations/codecs/avro",
                "derivations/codecs/thrift",
                "derivations/codecs/bson",
                "derivations/codecs/json",
                "derivations/codecs/message-pack",
                "derivations/codecs/protobuf",
                "derivations/codecs/xml",
              ],
            },
          ],
        },
        {
          type: "category",
          label: "Examples",
          collapsed: true,
          items: [
            "examples/mapping-dto-to-domain-object",
            "examples/combining-different-encoders",
          ],
        },
      ],
    },
  ],
};

module.exports = sidebars;
