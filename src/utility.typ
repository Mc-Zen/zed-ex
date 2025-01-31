
#let is-edge(child) = type(child) == dictionary and "nodes" in child
#let is-spider(child) = type(child) == dictionary and "kind" in child
#let is-curve-element(child) = type(child) == dictionary and "relative" in child
#let is-content(child) = type(child) == dictionary and "body" in child